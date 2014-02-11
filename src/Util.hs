{-# Language GADTs #-}

module Handy.Util where

import Data.Int (Int32)
import Data.Bits
import Handy.Instructions

bitmask24 :: Int32
bitmask24 = complement $ (255 :: Int32) `rotateR` 8

-- computeBranchOffset src := (SignExtend_30(signed_immed_24(src)) << 2)

-- This is semantically consistent with the ISA defined behaviour for arguments
-- to the B and BL instructions.
computeBranchOffset :: Argument Constant -> Argument Constant
computeBranchOffset (ArgC val) = ArgC result
                                 where src = bitmask24 .&. val
                                       se_src = if src `testBit` 23
                                                then src .|. (complement bitmask24)
                                                else src
                                       result = se_src `shiftL` 2
