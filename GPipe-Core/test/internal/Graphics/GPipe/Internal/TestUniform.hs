{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Graphics.GPipe.Internal.TestUniform where

import Control.Monad.Trans.Writer
import qualified Data.IntMap as Map

import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Uniform

import Test.Framework

withNewline s = s ++ "\n"

test_buildUDecl = do
    -- All types, no padding
    assertEqual
        (concatMap withNewline
            [ "float u0;"
            , "int u4;"
            , "bool u8;"
            , "uint u12;"
            , "whatever u16;"
            , "mat3x2 u20;"
            , "vec2 u24;"
            , "ivec3 u32;"
            , "uvec4 u44;"
            , "bool u60;"
            ])
        (snd . runWriter . buildUDecl $ Map.fromList
            [ (0, STypeFloat)
            , (4, STypeInt)
            , (8, STypeBool)
            , (12, STypeUInt)
            , (16, STypeDyn "whatever")
            , (20, STypeMat 2 3) -- Just 4?
            , (24, STypeVec 2)
            , (32, STypeIVec 3)
            , (44, STypeUVec 4)
            , (60, STypeGenerativeGeometry)
            ])
    -- With padding
    assertEqual
        (concatMap withNewline
            [ "float pad0;"
            , "int u4;"
            , "float pad8;"
            , "int u12;"
            , "float pad16;"
            , "float pad20;"
            , "int u24;"
            , "float pad28;"
            , "float pad32;"
            , "float pad36;"
            , "int u40;"
            ])
        (snd . runWriter . buildUDecl $ Map.fromList
            [ (4, STypeInt)
            , (12, STypeInt)
            , (24, STypeInt)
            , (40, STypeInt)
            ])
