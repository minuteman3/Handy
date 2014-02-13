{-# Language GADTs #-}

module Handy.Util where

import Data.Int (Int32)
import Data.Word (Word32,Word64)
import Data.Bits
import Handy.StatusRegister
import Prelude hiding (EQ,GT,LT)

bitmask :: Int -> Word32
bitmask i = 2^i - 1

isCarry :: Int32 -> Int32 -> Bool
isCarry a b = result `testBit` 32
              where a' = fromIntegral a :: Word64
                    b' = fromIntegral b :: Word64
                    result = a' + b' :: Word64

isOverflow :: Int32 -> Int32 -> Bool
isOverflow a b = (signum a) == (signum b) && (signum a) /= (signum (a + b))

