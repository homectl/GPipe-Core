{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Graphics.GPipe.Internal.TestCompiler
import {-@ HTF_TESTS @-} Graphics.GPipe.Internal.TestPrimitiveStream
import {-@ HTF_TESTS @-} Graphics.GPipe.Internal.TestUniform

main = htfMain htf_importedTests
