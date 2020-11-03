{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, FlexibleContexts #-}

module Graphics.GPipe.Internal.TestExpr where

import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.PrimitiveStream
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.GeometryStream

import Control.Arrow
import Control.Category
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Boolean
import qualified Data.IntMap as Map
import Prelude hiding (length, id, (.), (<*))
import Linear.V3

import Test.Framework

withNewline s = s ++ "\n"

test_while :: IO ()
test_while = do
    let
        total :: VInt
        total = snd $ while
            (\(i, _) -> i <* 3)
            (\(i, n) -> (i + 1, 10 * i + n))
            (0, 1)

        decls = tellGlobalLn "// hello"

        shaderExpr = unS total >> return ()

    -- The last two values aren’t meant to be evaluated (will trip over an
    -- undefined value otherwise) since there is no previous stage here.
    (source, unis, samps, inps, _, _) <- runExprM decls shaderExpr

    assertEqual
        (concatMap withNewline
            [ "#version 450"
            , "// hello;"
            , "void main() {"
            , "int t0;"
            , "int t1;"
            , "t0 = 0;" -- i <- 0
            , "t1 = 1;" -- n <- 1
            , "bool t2 = (0<3);"
            , "bool t3 = t2;"
            , "while(t3){"
            , "int t4 = (t0+1);"
            , "t0 = t4;" -- i = i + 1
            , "int t5 = (10*t0);" -- Variable 'i' (t0) has already been incremented.
            , "int t6 = (t5+t1);"
            , "t1 = t6;" -- n = 10 * i + n
            , "bool t7 = (t4<3);"
            , "t3 = t7;"
            , "}"
            , "}"
            ])
        source
