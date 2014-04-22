module Handy.Memory where

import Data.Word (Word8,Word16,Word32)
import Data.Bits
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M

type Memory = IntMap Word8

blankMemory :: Memory
blankMemory = M.empty

getByte :: Memory -> Word32 -> Word8
getByte mem a = M.findWithDefault 0 (fromIntegral a) mem

getHalfWord :: Memory -> Word32 -> Word16
getHalfWord mem a = upper .|. lower
        where upper = fromIntegral (mem `getByte` a) `shiftL` 8
              lower = fromIntegral $ mem `getByte` (a + 1)

getWord :: Memory -> Word32 -> Word32
getWord mem a = upper .|. lower
        where upper = fromIntegral (mem `getHalfWord` a) `shiftL` 16
              lower = fromIntegral $ mem `getHalfWord` (a + 2)

writeByte :: Memory -> Word32 -> Word8 -> Memory
writeByte mem a v = M.insert (fromIntegral a) v mem

writeHalfWord :: Memory -> Word32 -> Word16 -> Memory
writeHalfWord mem a v = result
        where  mem'   = writeByte mem a upper
               result = writeByte mem' (a+1) lower
               lower  = fromIntegral v
               upper  = fromIntegral (v `shiftR` 8)

writeWord :: Memory -> Word32 -> Word32 -> Memory
writeWord mem a v = result
        where mem'    = writeHalfWord mem a upper
              result  = writeHalfWord mem' (a+2) lower
              lower   = fromIntegral v
              upper   = fromIntegral (v `shiftR` 16)

getInstructionWord :: Memory -> Word32 -> [Word8]
getInstructionWord mem a = [mem `getByte` a
                           ,mem `getByte` (a + 1)
                           ,mem `getByte` (a + 2)
                           ,mem `getByte` (a + 3)]
