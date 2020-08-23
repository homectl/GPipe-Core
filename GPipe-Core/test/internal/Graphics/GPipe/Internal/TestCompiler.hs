module Graphics.GPipe.Internal.TestCompiler where

import Graphics.GPipe.Internal.Compiler

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

-- 3, 4, 2, 7, 9, 1, 8, 6
-- 0, 1, 2, 3, 4, 5, 6, 7

compilerTestGroup = testGroup "compile"

    [ testCase "1" $ [[0,1,2,3],[4,0,2],[2,5,6],[7]] @=? oldAllocateWhichGiveStrangeResults 10 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
    , testCase "2" $ [[0,1,2,3],[4,0,2],[2,0,0],[2]] @=? oldAllocateWhichGiveStrangeResults 5 [[3,4,2,7], [9,3,2], [2,1,8], [6]] -- Bad?
    , testCase "3" $ [[0,1,2,3],[4,0,2],[3,2,2],[0]] @=? oldAllocateWhichGiveStrangeResults 5 [[3,4,2,7], [9,3,2], [7,1,8], [6]] -- Bad?
    , testCase "4" $ [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? oldAllocateWhichGiveStrangeResults 1 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
    -- , testCase "5" $ [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? oldAllocateWhichGiveStrangeResults 0 [[3,4,2,7], [9,3,2], [2,1,8], [6]] -- Boom!
    , testCase "6" $ [[0,1,2,2,2,2]] @=? oldAllocateWhichGiveStrangeResults 3 [[1,2,3,4,5,6]]
    , testCase "7" $ [[0,1,2,3,3,3]] @=? oldAllocateWhichGiveStrangeResults 4 [[1,2,3,4,5,6]]
    , testCase "8" $ [[0,1,2,3,4,4]] @=? oldAllocateWhichGiveStrangeResults 5 [[1,2,3,4,5,6]]
    
    , testCase "1" $ [[0,1,2,3],[4,0,2],[2,5,6],[7]] @=? allocateConsecutiveIndexes 10 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
    -- , testCase "2" $ [[0,1,2,3],[4,0,2],[2,0,0],[0]] @=? allocateConsecutiveIndexes 5 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
    -- , testCase "3" $ [[0,1,2,3],[4,0,2],[3,0,0],[0]] @=? allocateConsecutiveIndexes 5 [[3,4,2,7], [9,3,2], [7,1,8], [6]]
    -- , testCase "4" $ [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? allocateConsecutiveIndexes 1 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
    -- , testCase "5" $ [[0,0,0,0],[0,0,0],[0,0,0],[0]] @=? allocateConsecutiveIndexes 0 [[3,4,2,7], [9,3,2], [2,1,8], [6]]
    ]

-- Tester invariance à un décalage près.
