{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.GPipe.Internal.TransformFeedback where

import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Format
import Graphics.GPipe.Internal.FragmentStream
import Graphics.GPipe.Internal.GeometryStream
import Graphics.GPipe.Internal.PrimitiveStream
import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Texture

import Graphics.GL.Core45
import Graphics.GL.Types

import Data.IORef
import Data.IntMap.Lazy (insert)
import Foreign ({-Ptr, plusPtr, castPtr,-} nullPtr {-, sizeOf, with-})
import Foreign.C.String
import Foreign.Marshal
import Control.Monad.Trans.State
import Graphics.GPipe.Internal.Debug

drawNothing :: forall p a s c ds os f. (PrimitiveTopology p, VertexInput a, FragmentInputFromGeometry p (VertexFormat a), GeometryExplosive (VertexFormat a))
    => Window os c ds
    -- Output feedback buffers should remain black boxes until synchronized
    -- which won't be necessary when using glDrawTransformFeedback (add a flag
    -- for it?).
    -> Buffer os a
    -- maxVertices
    -> Int
    -- We should use a primitive (vertex) stream too, but the way we deal
    -- currently with modular stages is not flexible enough and we stick with
    -- geometry stream.
    -> GeometryStream (GGenerativeGeometry p (VertexFormat a))
    -> Shader os s ()
drawNothing w buffer maxVertices gs = Shader $ tellDrawcalls w gs buffer maxVertices

tellDrawcalls :: forall p a s c ds os. (PrimitiveTopology p, VertexInput a, FragmentInputFromGeometry p (VertexFormat a), GeometryExplosive (VertexFormat a))
    => Window os c ds
    -> GeometryStream (GGenerativeGeometry p (VertexFormat a))
    -> Buffer os a
    -> Int
    -> ShaderM s ()
tellDrawcalls w (GeometryStream xs) buffer maxVertices =  mapM_ f xs where
    f (x, gsd@(GeometryStreamData n layoutName _)) = do

        let shaderDeclarations = (evalState (declareGeometry (undefined :: VertexFormat a)) 0)
            varyings = evalState (enumerateVaryings (undefined :: VertexFormat a)) 0
            varyingCount = length varyings
            bufferMode = GL_INTERLEAVED_ATTRIBS
            io s pName = do
                names <- mapM newCString (traceList "varyings" varyings)
                withArray names $ \a -> do
                    glTransformFeedbackVaryings pName (fromIntegral varyingCount) a bufferMode
                mapM_ free names

        tellDrawcall $ makeDrawcall w buffer gsd shaderDeclarations $ do
            declareGeometryLayout layoutName (toLayoutOut (undefined :: p)) maxVertices
            x' <- unS x
            return ()

        modifyRenderIO (\s -> s { transformFeedbackToRenderIO = insert n io (transformFeedbackToRenderIO s) } )

makeDrawcall :: forall a s c ds os. (VertexInput a) -- , GeometryExplosive a
    => Window os c ds
    -> Buffer os a
    -> GeometryStreamData
    -> GlobDeclM ()
    -> ExprM ()
    -> IO (Drawcall s)
makeDrawcall w buffer (GeometryStreamData geoN _ (PrimitiveStreamData primN ubuff)) shaderDeclarations shader = do
    (gsource, gunis, gsamps, _, prevShaderDeclarations, prevShader) <- runExprM shaderDeclarations shader
    (vsource, vunis, vsamps, vinps, _, _) <- runExprM prevShaderDeclarations prevShader
    bufferName <- readIORef (bufName buffer)
    return $ Drawcall
        (const (Left (getWinName w), return ()))
        (Just bufferName)
        primN
        (Just geoN)
        vsource (Just gsource) Nothing
        vinps
        vunis vsamps
        gunis gsamps
        [] []
        ubuff

-- To be compiled with (glEnable GL_RASTERIZER_DISCARD).

{-
glBindTransformFeedback GL_TRANSFORM_FEEDBACK bname
glBindBuffer GL_ARRAY_BUFFER bname
glBufferData GL_ARRAY_BUFFER 100 nullPtr GL_STATIC_READ
glBindBufferBase GL_TRANSFORM_FEEDBACK_BUFFER 0 bname
-}
--------------------------------------------------------------------------------

data ShaderStageInput = ShaderStageInput
    {    -- The output declarations to include in the shader's source.
        outputDeclarations :: GlobDeclM ()
        -- The expression to evaluate as a source using variables to be provided
        -- by a previous shader (or buffer object). The top level of this
        -- expression is expected (how exactly?) to assign a value to the output
        -- variables declared above.
    ,   expression :: ExprM ()
    }

data ShaderStageOutput = ShaderStageOutput
    {   source :: String -- ^ The shader GLSL source to be compiled.
    ,   uniforms :: [Int] -- ^ The uniforms used in this shader.
    ,   samplers :: [Int] -- ^ The samplers used in this shader.
    ,   inputs :: [Int] -- ^ The input variables used in this shader.
    ,   previousDeclarations :: GlobDeclM () -- ^ The output declations to include in the previous shader to provide the needed input variables.
    ,   prevExpression :: ExprM () -- ^ The expression to evaluate in order to produce the previous shader.
    }

evaluateExpression :: [ExprM ()] -> ExprM () -> GlobDeclM () -> IO ShaderStageOutput
evaluateExpression staticExpressions expression requiredOutputDeclarations = do
    (s, u, ss, is, pds, pe) <- runExprM requiredOutputDeclarations expression
    case staticExpressions of
        (se:ses) -> evaluateExpression ses (pe >> se) pds
        [] -> return $ ShaderStageOutput s u ss is pds pe
