{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.GPipe.Internal.TransformFeedback where

import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.GeometryStream
import Graphics.GPipe.Internal.PrimitiveStream
import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Debug

import Graphics.GL.Core45
import Graphics.GL.Types

import Data.IORef
import Data.IntMap.Lazy (insert)
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Trans.State

drawNothing :: forall p a s c ds os f. (PrimitiveTopology p, VertexInput a, FragmentInputFromGeometry p (VertexFormat a), GeometryExplosive (VertexFormat a))
    => Window os c ds
    -- Output feedback buffers should remain black boxes until synchronized
    -- which won't be necessary when using glDrawTransformFeedback (add a flag
    -- for it?).
    -> (s -> Buffer os a)
    -- maxVertices
    -> Int
    -- We should use a primitive (vertex) stream too, but the way we deal
    -- currently with modular stages is not flexible enough and we stick with
    -- geometry stream.
    -> GeometryStream (GGenerativeGeometry p (VertexFormat a))
    -> Shader os s ()
drawNothing w getTransformFeedbackBuffer maxVertices gs = Shader $ tellDrawcalls w gs getTransformFeedbackBuffer maxVertices

tellDrawcalls :: forall p a s c ds os. (PrimitiveTopology p, VertexInput a, FragmentInputFromGeometry p (VertexFormat a), GeometryExplosive (VertexFormat a))
    => Window os c ds
    -> GeometryStream (GGenerativeGeometry p (VertexFormat a))
    -> (s -> Buffer os a)
    -> Int
    -> ShaderM s ()
tellDrawcalls w (GeometryStream xs) getTransformFeedbackBuffer maxVertices =  mapM_ f xs where
    f (x, gsd@(GeometryStreamData n layoutName _)) = do

        let shaderDeclarations = (evalState (declareGeometry (undefined :: VertexFormat a)) 0)
            varyings = evalState (enumerateVaryings (undefined :: VertexFormat a)) 0
            varyingCount = length varyings
            bufferMode = GL_INTERLEAVED_ATTRIBS
            io s pName = do
                names <- mapM newCString varyings
                withArray names $ \a -> do
                    glTransformFeedbackVaryings pName (fromIntegral varyingCount) a bufferMode
                mapM_ free names

        tellDrawcall $ makeDrawcall w getTransformFeedbackBuffer gsd shaderDeclarations $ do
            declareGeometryLayout layoutName (toLayoutOut (undefined :: p)) maxVertices
            x' <- unS x
            return ()

        modifyRenderIO (\s -> s { transformFeedbackToRenderIO = insert n io (transformFeedbackToRenderIO s) } )

makeDrawcall :: forall a s c ds os. (VertexInput a) -- , GeometryExplosive a
    => Window os c ds
    -> (s -> Buffer os a)
    -> GeometryStreamData
    -> GlobDeclM ()
    -> ExprM ()
    -> IO (Drawcall s)
makeDrawcall w getTransformFeedbackBuffer (GeometryStreamData geoN _ (PrimitiveStreamData primN ubuff)) shaderDeclarations shader = do
    (gsource, gunis, gsamps, _, prevShaderDeclarations, prevShader) <- runExprM shaderDeclarations shader
    (vsource, vunis, vsamps, vinps, _, _) <- runExprM prevShaderDeclarations prevShader
    let getTransformFeedbackBufferName = \s -> do
            let buffer = getTransformFeedbackBuffer s
            bName <- readIORef (bufName buffer)
            let tfRef = bufTransformFeedback buffer
            tf <- readIORef tfRef
            tfName <- case tf of
                Just name -> return name
                Nothing -> do
                    name <- alloca $ \ptr -> do
                        glGenTransformFeedbacks 1 ptr
                        peek ptr
                    writeIORef tfRef (Just name)
                    --liftNonWinContextIO $ (addContextFinalizer tfRef $ with name (glDeleteTransformFeedbacks 1))
                    return name
            {-
            glBindTransformFeedback GL_TRANSFORM_FEEDBACK_BUFFER tfName
            glBindBufferBase GL_TRANSFORM_FEEDBACK_BUFFER 0 bName
            glBindTransformFeedback GL_TRANSFORM_FEEDBACK_BUFFER 0
            -}
            return (bName, tfName)
    return $ Drawcall
        (const (Left (getWinName w), return ()))
        (Just getTransformFeedbackBufferName)
        primN
        (Just geoN)
        vsource (Just gsource) Nothing
        vinps
        vunis vsamps
        gunis gsamps
        [] []
        ubuff
