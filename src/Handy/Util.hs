{-# Language GADTs #-}

module Handy.Util where

import Data.Int (Int32)
import Data.Word (Word32,Word64)
import Data.Bits
import Handy.Instructions
import Handy.StatusRegister
import Prelude hiding (EQ,GT,LT)

bitmask :: Int -> Word32
bitmask i = 2^i - 1

-- computeBranchOffset src := (SignExtend_30(signed_immed_24(src)) << 2)

-- This is semantically consistent with the ISA defined behaviour for arguments
-- to the B and BL instructions.
computeBranchOffset :: Argument Constant -> Argument Constant
computeBranchOffset (ArgC val) = ArgC result
                                 where src = (fromIntegral $ mask) .&. val
                                       se_src = if src `testBit` 23
                                                then src .|. (fromIntegral $ complement mask)
                                                else src
                                       result = se_src `shiftL` 2
                                       mask = bitmask 24

isCarry :: Int32 -> Int32 -> Bool
isCarry a b = result `testBit` 32
              where a' = fromIntegral a :: Word64
                    b' = fromIntegral b :: Word64
                    result = a' + b' :: Word64

isOverflow :: Int32 -> Int32 -> Bool
isOverflow a b = (signum a) == (signum b) && (signum a) /= (signum (a + b))

checkCondition :: Condition -> StatusRegister -> Bool
checkCondition AL _ = True
checkCondition EQ cpsr = zero cpsr
checkCondition CS cpsr = carry cpsr
checkCondition MI cpsr = negative cpsr
checkCondition VS cpsr = overflow cpsr
checkCondition HI cpsr = (carry cpsr) && (not $ zero cpsr)
checkCondition GE cpsr = (negative cpsr) == (overflow cpsr)
checkCondition NE cpsr = not $ checkCondition EQ cpsr
checkCondition CC cpsr = not $ checkCondition CS cpsr
checkCondition PL cpsr = not $ checkCondition MI cpsr
checkCondition VC cpsr = not $ checkCondition VS cpsr
checkCondition LS cpsr = not $ checkCondition HI cpsr
checkCondition LT cpsr = not $ checkCondition GE cpsr
checkCondition GT cpsr = (not $ zero cpsr) && (checkCondition GE cpsr)
checkCondition LE cpsr = not $ checkCondition GT cpsr
checkCondition HS cpsr = checkCondition CS cpsr
checkCondition LO cpsr = checkCondition CC cpsr
