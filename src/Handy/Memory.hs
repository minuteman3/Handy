module Handy.Memory where

import Handy.Instructions
import Handy.Registers
import Data.Word
import Data.Bits
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M

type Memory = [Instruction]

type RealMemory = IntMap Word8

blankMemory :: RealMemory
blankMemory = M.empty

getByte :: RealMemory -> Word32 -> Word8
getByte mem a = M.findWithDefault 0 (fromIntegral a) mem

getHalfWord :: RealMemory -> Word32 -> Word16
getHalfWord mem a = upper .|. lower where upper = (fromIntegral $ mem `getByte` a) `shiftL` 8
                                          lower = fromIntegral $ mem `getByte` (a + 1)

getWord :: RealMemory -> Word32 -> Word32
getWord mem a = upper .|. lower where upper = (fromIntegral $ mem `getHalfWord` a) `shiftL` 16
                                      lower = fromIntegral $ mem `getHalfWord` (a + 2)

writeByte :: RealMemory -> Word32 -> Word8 -> RealMemory
writeByte mem a v = M.insert (fromIntegral a) v mem

writeHalfWord :: RealMemory -> Word32 -> Word16 -> RealMemory
writeHalfWord mem a v = result where
                            mem'   = (writeByte mem a upper)
                            result = (writeByte mem' (a+1) lower)
                            lower  = fromIntegral v
                            upper  = fromIntegral $ (v `shiftR` 8)

writeWord :: RealMemory -> Word32 -> Word32 -> RealMemory
writeWord mem a v = result where
                        mem'    = (writeHalfWord mem a upper)
                        result  = (writeHalfWord mem' (a+2) lower)
                        lower   = fromIntegral v
                        upper   = fromIntegral $ (v `shiftR` 16)

getInstructionWord :: RealMemory -> Word32 -> [Word8]
getInstructionWord mem a = [mem `getByte` a
                           ,mem `getByte` (a + 1)
                           ,mem `getByte` (a + 2)
                           ,mem `getByte` (a + 3)]
