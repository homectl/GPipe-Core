{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses #-}

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

-- Add a transform feedback at the end of the vertex stage.
withTransformFeedback :: forall p q a b os s. (PrimitiveTopology p, VertexInput a, PrimitiveTopology q, VertexInput b)
    -- We should be to use a primitive (vertex) stream too, but the way we deal
    -- currently with modular stage is not flexible enough and we stick with
    -- geometry stream.
    => GeometryStream (GGenerativeGeometry p (VPos, a))
    -- Cherry picks what we want to collect in the feedback buffer.
    -> (q, (VPos, a) -> b)
    -- Output feedback buffers should remain black boxes until synchronized
    -- which won't be necessary when using glDrawTransformFeedback (add a flag
    -- for it?).
    -> Buffer os b
    -- Chaining another transform feedback with the returned stream is sound but
    -- not allowed for now.
    -> GeometryStream (GGenerativeGeometry p (VPos, a))
withTransformFeedback = undefined

-- Create a GPipeShader without rasterization stage, just for the sake of
-- producing feedback.
drawNothing :: forall p a os s. (PrimitiveTopology p, VertexInput a)
    => GeometryStream (GGenerativeGeometry p (VPos, a))
    -> Shader os s ()
drawNothing = undefined

{-
withTransformFeedback :: (s -> Buffer os a) -> GeometryStream a -> GeometryStream b
withTransformFeedback f (GeometryStream xs) = GeometryStream $ map (\(a, (ps, d)) -> let (b, ps') = f a (fromMaybe (scalarS' "1") ps) in (b, (Just ps', d))) xs

tellTransformWithFeedbackCalls :: VertexInput a
    =>  GeometryStream a
    ->  (a -> (ExprM (), GlobDeclM (), s -> (Buffer os a)))
    ->  ShaderM s ()
tellTransformWithFeedbackCalls (GeometryStream xs) f = do
    let g (x, fd) = tellTransformWithFeedbackCall $ makeTransformWithFeedbackCall (f x) fd
    mapM_ g xs

tellTransformWithFeedbackCall :: IO (TransformWithFeedbackCall s) -> ShaderM s ()
tellTransformWithFeedbackCall fc = ShaderM $ lift $ tell ([fc], mempty)

-- forall os s a.
makeTransformWithFeedbackCall :: VertexInput a
    =>  ( ExprM () -- shaderExpression
        , GlobDeclM () -- outputShaderDeclarations
        , s -> Buffer os a -- getBufferObject (only one, no interleaving)
        )
    ->  GeometryStreamData -> IO (TransformWithFeedbackCall s)
makeTransformWithFeedbackCall (shaderExpression, outputShaderDeclarations, getBufferObject) (GeometryStreamData layoutName (PrimitiveStreamData primitiveName uBufferSize)) = do
    (gSource, gUniforms, gSamplers, _, prevShaderDeclarations, prevShader) <- runExprM outputShaderDeclarations shaderExpression
    (vSource, vUniforms, vSamplers, vInputs, _, _) <- runExprM prevShaderDeclarations prevShader
    return $ TransformWithFeedbackCall (return aWayToRetrieveTheBufferRefName) primitiveName vSource (Just gSource) vInputs vUniforms vSamplers gUniforms gSamplers uBufferSize

aWayToRetrieveTheBufferRefName = -1
-}

{-
void glTransformFeedbackVaryings(GLuint program​, GLsizei count​, const char **varyings​, GLenum bufferMode​=GL_INTERLEAVED_ATTRIBS);

void glBeginTransformFeedback(GLenum primitiveMode​)
void glPauseTransformFeedback()
void glResumeTransformFeedback()
void glEndTransformFeedback()

glDrawTransformFeedback
glDrawTransformFeedbackInstanced
glDrawTransformFeedbackStream
glDrawTransformFeedbackStreamInstanced
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

evaluateExpression :: ExprM () -> GlobDeclM () -> IO ShaderStageOutput
evaluateExpression expression requiredOutputDeclarations = do
    (a, b, c, d, e, f) <- runExprM requiredOutputDeclarations expression
    return $ ShaderStageOutput a b c d e f
