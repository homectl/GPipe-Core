module Main
    ( main
    ) where

import Control.Exception
import Data.Maybe

import Graphics.GPipe.Internal.Compiler

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

compileTestGroup = testGroup "compile"
    [   testCase "testAllocate1" testAllocate1
    ,   testCase "testAllocate2" testAllocate2
    ,   testCase "testAllocate2b" testAllocate2b
    ,   testCase "testAllocate3" testAllocate3
    ,   testCase "testAllocate4" testAllocate4
    ,   testCase "testAllocateConsecutiveIndexes1" testAllocateConsecutiveIndexes1
    ,   testCase "testAllocateConsecutiveIndexes2" testAllocateConsecutiveIndexes2
    ,   testCase "testAllocateConsecutiveIndexes2b" testAllocateConsecutiveIndexes2
    ,   testCase "testAllocateConsecutiveIndexes3" testAllocateConsecutiveIndexes3
    ,   testCase "testAllocateConsecutiveIndexes4" testAllocateConsecutiveIndexes4
    ]

-- 3, 4, 2, 7, 9, 1, 8, 6
-- 0, 1, 2, 3, 4, 5, 6, 7

testAllocate1 = [[0,1,2,3],[4,0,2],[2,5,6],[7]] @=? oldAllocateWhichGiveStrangeResults 10 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
testAllocate2 = [[0,1,2,3],[4,0,2],[2,0,0],[2]] @=? oldAllocateWhichGiveStrangeResults 5 [[3,4,2,7], [9,3,2], [2,1,8], [6]] -- Bad!
testAllocate2b = [[0,1,2,3],[4,0,2],[3,2,2],[0]] @=? oldAllocateWhichGiveStrangeResults 5 [[3,4,2,7], [9,3,2], [7,1,8], [6]] -- Bad!
testAllocate3 = [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? oldAllocateWhichGiveStrangeResults 1 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
testAllocate4 = [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? oldAllocateWhichGiveStrangeResults 0 [[3,4,2,7], [9,3,2], [2,1,8], [6]] -- Boom!

testAllocateConsecutiveIndexes1 = [[0,1,2,3],[4,0,2],[2,5,6],[7]] @=? allocateConsecutiveIndexes 10 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
testAllocateConsecutiveIndexes2 = [[0,1,2,3],[4,0,2],[2,0,0],[0]] @=? allocateConsecutiveIndexes 5 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
testAllocateConsecutiveIndexes2b = [[0,1,2,3],[4,0,2],[3,0,0],[0]] @=? allocateConsecutiveIndexes 5 [[3,4,2,7], [9,3,2], [7,1,8], [6]]
testAllocateConsecutiveIndexes3 = [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? allocateConsecutiveIndexes 1 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
testAllocateConsecutiveIndexes4 = [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? allocateConsecutiveIndexes 0 [[3,4,2,7], [9,3,2], [2,1,8], [6]]

-- Tester invariance à un décalage près

main = defaultMain
    [ testGroup "GPipe"
        [   compileTestGroup
        ]
    ]
