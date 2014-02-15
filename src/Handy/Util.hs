{-# Language GADTs #-}

module Handy.Util where

import Data.Int (Int32)
import Data.Word (Word8,Word32,Word64)
import Data.Bits
import Prelude hiding (EQ,GT,LT)

bitmask :: Int -> Word32
bitmask i = 2^i - 1

isCarry :: (Word64 -> Word64 -> Word64) -> Int32 -> Int32 -> Bool
isCarry op a b = result `testBit` 32
                 where a' = fromIntegral a :: Word64
                       b' = fromIntegral b :: Word64
                       result = a' `op` b' :: Word64

isOverflow :: Int32 -> Int32 -> Bool
isOverflow a b = signum a == signum b && signum a /= signum (a + b)

chunkWord32 :: Word32 -> [Word8]
chunkWord32 word = map fromIntegral [a,b,c,d] where a = word `shiftR` 24
                                                    b = (word `shiftR` 16) .&. bitmask 8
                                                    c = (word `shiftR` 8)  .&. bitmask 8
                                                    d = word .&. bitmask 8
