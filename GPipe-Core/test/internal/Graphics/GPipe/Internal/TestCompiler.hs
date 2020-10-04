{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Graphics.GPipe.Internal.TestCompiler where

import Graphics.GPipe.Internal.Compiler

import Test.Framework

-- 3, 4, 2, 7, 9, 1, 8, 6
-- 0, 1, 2, 3, 4, 5, 6, 7

test_compile1 = assertEqual [[0,1,2,3],[4,0,2],[2,5,6],[7]] (oldAllocateWhichGiveStrangeResults 10 [[3,4,2,7], [9,3,2], [2,1,8], [6]])
test_compile2 = assertEqual [[0,1,2,3],[4,0,2],[2,0,0],[2]] (oldAllocateWhichGiveStrangeResults 5 [[3,4,2,7], [9,3,2], [2,1,8], [6]]) -- Bad?
test_compile3 = assertEqual [[0,1,2,3],[4,0,2],[3,2,2],[0]] (oldAllocateWhichGiveStrangeResults 5 [[3,4,2,7], [9,3,2], [7,1,8], [6]]) -- Bad?
test_compile4 = assertEqual [[0,0,0,0],[0,0,0],[0,0,0],[0]] (oldAllocateWhichGiveStrangeResults 1 [[3,4,2,7], [9,3,2], [2,1,8], [6]])
-- test_compileX = assertEqual [[0,0,0,0],[0,0,0],[0,0,0],[0]] (oldAllocateWhichGiveStrangeResults 0 [[3,4,2,7], [9,3,2], [2,1,8], [6]]) -- Boom!
test_compile5 = assertEqual [[0,1,2,2,2,2]] (oldAllocateWhichGiveStrangeResults 3 [[1,2,3,4,5,6]])
test_compile6 = assertEqual [[0,1,2,3,3,3]] (oldAllocateWhichGiveStrangeResults 4 [[1,2,3,4,5,6]])
test_compile7 = assertEqual [[0,1,2,3,4,4]] (oldAllocateWhichGiveStrangeResults 5 [[1,2,3,4,5,6]])

test_compile8 = assertEqual [[0,1,2,3],[4,0,2],[2,5,6],[7]] (allocateConsecutiveIndexes 10 [[3,4,2,7], [9,3,2], [2,1,8], [6]])
-- test_compileX = assertEqual [[0,1,2,3],[4,0,2],[2,0,0],[0]] (allocateConsecutiveIndexes 5 [[3,4,2,7], [9,3,2], [2,1,8], [6]])
-- test_compileX = assertEqual [[0,1,2,3],[4,0,2],[3,0,0],[0]] (allocateConsecutiveIndexes 5 [[3,4,2,7], [9,3,2], [7,1,8], [6]])
-- test_compileX = assertEqual [[0,0,0,0],[0,0,0],[0,0,0],[0]] (allocateConsecutiveIndexes 1 [[3,4,2,7], [9,3,2], [2,1,8], [6]])
-- test_compileX = assertEqual [[0,0,0,0],[0,0,0],[0,0,0],[0]] (allocateConsecutiveIndexes 0 [[3,4,2,7], [9,3,2], [2,1,8], [6]])

-- Tester invariance à un décalage près.
