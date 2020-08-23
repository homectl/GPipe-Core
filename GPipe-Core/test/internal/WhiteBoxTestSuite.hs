module Main
    ( main
    ) where

import Graphics.GPipe.Internal.TestCompiler

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

main = defaultMain
    [ testGroup "GPipe"
        [   compilerTestGroup
        ]
    ]
