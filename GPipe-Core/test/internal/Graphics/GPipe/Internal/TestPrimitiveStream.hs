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
        ToVertex
            (Kleisli uWriter) -- To set the uniform content (for the literal values, not the one buffered).
            (Kleisli makeV) -- To create (and declare) the input variable in the shader.
            (Kleisli makeBind) -- To construct the VAO.
            = toVertex :: ToVertex (B2 Float) (VertexFormat (B2 Float)) -- Select the ToVertex to translate 'a' into a 'VertexFormat a'.

        -- Stream, varying, the shader could only depend on the streamed type, not any particular value.
        err = error $
            "Creating values that are dependant on the actual HostFormat values," ++
            " this is not allowed since it doesn't allow static creation of shaders"

        numberOfComponentsInStoredType = 22
        uniOffset = undefined
        offToStype0 = mempty

        (x, (_, uSize, offToStype)) = runReader
            (runStateT (makeV err) (numberOfComponentsInStoredType, uniOffset, offToStype0))
            (useUniform (buildUDecl offToStype) 0) -- 0 is special blockname for the one used by primitive stream

        d = tellGlobalLn "// hello"

        e = mapM unS x >>= \(V2 s1 s2) -> tellAssignment' "out_name" (concat ["(", s1, ",", s2, ")"])

    (source, unis, samps, inps, prevDecls, prevSs) <- runExprM d e

    assertEqual
        (concatMap withNewline
            [ "#version 450"
            , "// hello;"
            , "in vec2 in22;"
            , "void main() {"
            , "vec2 t0 = in22;"
            , "out_name = (t0.x,t0.y);"
            , "}"
            ])
        source
