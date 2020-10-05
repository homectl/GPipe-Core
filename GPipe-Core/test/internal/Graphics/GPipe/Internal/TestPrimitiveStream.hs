{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Graphics.GPipe.Internal.TestPrimitiveStream where

import Control.Arrow
import Control.Category
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import qualified Data.IntMap as Map
import Prelude hiding (length, id, (.))
import Linear.V2

import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Uniform
import Graphics.GPipe.Internal.PrimitiveStream

import Test.Framework

withNewline s = s ++ "\n"

test_toVertex = do
    let
        -- VertexFormat (B2 Float) = V2 VFloat
        ToVertex
            (Kleisli uWriter) -- To set the uniform vertex content, undefined for "regular" a vertex value.
            (Kleisli makeV) -- To declare the input variable in the shader.
            (Kleisli makeBind) -- To bind the underlying VAO.
            = toVertex :: ToVertex (B2 Float) (VertexFormat (B2 Float)) -- Select the ToVertex to translate 'a' into a 'VertexFormat a'.

        -- Stream, varying, the shader could only depend on the streamed type, not any particular value.
        -- In other words: makeV :: type_structure_only_not_its_value(B2 Float) -> V2 VFloat
        notToBeEvaluated = error $
            "Creating values that are dependant on the actual HostFormat values," ++
            " this is not allowed since it doesn't allow static creation of shaders"

        nextIndex = 22
        uniOffset = undefined
        offToStype0 = mempty

        (x, (_, uSize, offToStype)) = runReader
            (runStateT (makeV notToBeEvaluated) (nextIndex, uniOffset, offToStype0))
            (useUniform (buildUDecl offToStype) undefined)

        decls = tellGlobalLn "// hello"

        shaderExpr = mapM unS x >>= \(V2 s1 s2) -> return ()

    -- The last two values aren’t meant to be evaluated (will trip over an
    -- undefined value otherwise) since there is no previous stage here.
    (source, unis, samps, inps, _, _) <- runExprM decls shaderExpr

    let uDecl = snd . runWriter . buildUDecl $ offToStype
    assertEqual "" uDecl

    assertEqual
        (concatMap withNewline
            [ "#version 450"
            , "// hello;"
            , "in vec2 in22;"
            , "void main() {"
            , "vec2 t0 = in22;" -- Simply to create alias with a t(emporary) value name?
            , "}"
            ])
        source

    assertEqual ([] :: [Int]) unis
    assertEqual ([] :: [Int]) samps
    assertEqual [nextIndex] inps

    return ()

-- TODO Compare with regular Uniform to understand what it is.
test_toUniformVertex = do
    let
        ToVertex
            (Kleisli uWriter) -- To inject the value in the special uniform buffer.
            (Kleisli makeV) -- To declare the input variable in the shader.
            (Kleisli makeBind) -- Undefined for a uniform vertex value.
            = toVertex :: ToVertex Float (VertexFormat Float)

        notToBeEvaluated = error "Forbidden"

        nextIndex = undefined
        uniOffset = 8 -- multiple of 4
        offToStype0 = mempty

        (x, (_, uSize, offToStype)) = runReader
            (runStateT (makeV notToBeEvaluated) (nextIndex, uniOffset, offToStype0))
            (useUniform (buildUDecl offToStype) 33) -- 33 (0 in principle) is special blockname for the one used by primitive stream

        decls = tellGlobalLn "// hello"

        shaderExpr = unS x >>= \s -> return ()

    (source, unis, samps, inps, _, _) <- runExprM decls shaderExpr

    let uDecl = snd . runWriter . buildUDecl $ offToStype
    assertEqual
        (concatMap withNewline
            [ "float pad0;"
            , "float pad4;"
            , "float u8;"
            ])
        uDecl

    assertEqual
        (concatMap withNewline
            [ "#version 450"
            , "// hello;"
            , "layout(std140) uniform uBlock33 {"
            , uDecl ++ "} u33;"
            , "void main() {"
            -- Why no t(emporary) value name here?
            , "}"
            ])
        source

    assertEqual ([33] :: [Int]) unis
    assertEqual ([] :: [Int]) samps
    assertEqual [] inps

    return ()
