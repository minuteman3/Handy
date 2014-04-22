{-# Language GADTs #-}
module Handy.ALU where

import Prelude hiding (LT,GT,EQ)
import Handy.Instructions
import Handy.StatusRegister
import Handy.Registers
import Handy.Util
import Data.Int  (Int32,Int64)
import Data.Word (Word32,Word64)
import Data.Bits

umul :: Word64 -> Word64 -> Word64
umul = (*)

smul :: Int64 -> Int64 -> Int64
smul = (*)

compute :: Instruction
        -> RegisterFile
        -> StatusRegister
        -> (RegisterFile, StatusRegister)
compute i rf sr = (rf',sr'') where (rf',sr') = compute' i rf sr
                                   sr''      = case getS i of
                                                   S   -> sr'
                                                   NoS -> sr

compute' :: Instruction
         -> RegisterFile
         -> StatusRegister
         -> (RegisterFile, StatusRegister)

compute' (MOV cond _ dest src shft) rf sr =
    computeArith const dest arg arg cond sr' rf setSRarith1
    where arg                = ArgC shiftresult
          (shiftresult, sr') = computeShift src shft rf sr

compute' (MVN cond _ dest src shft) rf sr =
    computeArith (const.complement) dest arg arg cond sr' rf setSRarith1
    where arg                = ArgC shiftresult
          (shiftresult, sr') = computeShift src shft rf sr

compute' (MUL cond _ dest src1 src2) rf sr =
    computeArith (*) dest src1 src2 cond sr rf setSRarith1

compute' (MLA cond _ dest src1 src2 src3) rf sr =
    computeArith (+) dest (ArgR dest) src3 cond sr' rf' setSRarith1
    where (rf',sr') = computeArith (*) dest src1 src2 cond sr rf setSRarith1

compute' (SMULL cond _ dest1 dest2 src1 src2) rf sr =
    computeLongArith smul dest1 dest2 src1 src2 cond sr rf setSRarithLong

compute' (SMLAL cond _ dest1 dest2 src1 src2) rf sr =
    computeLongArithAccum smul dest1 dest2 src1 src2 cond sr rf setSRarithLong

compute' (UMULL cond _ dest1 dest2 src1 src2) rf sr =
    computeLongArith umul dest1 dest2 src1 src2 cond sr rf setSRarithLong

compute' (UMLAL cond _ dest1 dest2 src1 src2) rf sr =
    computeLongArithAccum umul dest1 dest2 src1 src2 cond sr rf setSRarithLong

compute' (ADD cond _ dest src1 src2 shft) rf sr =
    computeArith (+) dest src1 arg2 cond sr rf setSRarith2
    where arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (ADC cond _ dest src1 src2 shft) rf sr =
    computeArith (computeAdc sr) dest src1 arg2 cond sr rf setSRarith2
    where arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (SUB cond _ dest src1 src2 shft) rf sr =
    computeArith (-) dest src1 arg2 cond sr rf setSRarith3
    where arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (SBC cond _ dest src1 src2 shft) rf sr =
    computeArith (computeSbc sr) dest src1 arg2 cond sr rf setSRarith3
    where arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (RSB cond _ dest src1 src2 shft) rf sr =
    computeArith (-) dest arg2 src1 cond sr rf setSRarith3
    where arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (RSC cond _ dest src1 src2 shft) rf sr =
    computeArith (computeSbc sr) dest arg2 src1 cond sr rf setSRarith3
    where arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (CMP cond src1 src2 shft) rf sr = (rf,sr')
    where (_,sr')          = computeArith (-) None src1 arg2 cond sr rf setSRarith3
          arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (CMN cond src1 src2 shft) rf sr = (rf,sr')
    where (_,sr')          = computeArith (+) None src1 arg2 cond sr rf setSRarith2
          arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (TST cond src1 src2 shft) rf sr = (rf,sr')
    where (_,sr')          = computeArith (.&.) None src1 arg2 cond sr rf setSRarith1
          arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (TEQ cond src1 src2 shft) rf sr = (rf,sr')
    where (_,sr')          = computeArith xor None src1 arg2 cond sr rf setSRarith1
          arg2             = ArgC shiftresult
          (shiftresult, _) = computeShift src2 shft rf sr

compute' (AND cond _ dest src1 src2 shft) rf sr =
    computeArith (.&.) dest src1 arg2 cond sr' rf setSRarith1
    where arg2               = ArgC shiftresult
          (shiftresult, sr') = computeShift src2 shft rf sr

compute' (BIC cond _ dest src1 src2 shft) rf sr =
    computeArith computeBic dest src1 arg2 cond sr' rf setSRarith1
    where arg2               = ArgC shiftresult
          (shiftresult, sr') = computeShift src2 shft rf sr

compute' (ORR cond _ dest src1 src2 shft) rf sr =
    computeArith (.|.) dest src1 arg2 cond sr' rf setSRarith1
    where arg2               = ArgC shiftresult
          (shiftresult, sr') = computeShift src2 shft rf sr

compute' (EOR cond _ dest src1 src2 shft) rf sr =
    computeArith xor dest src1 arg2 cond sr' rf setSRarith1
    where arg2               = ArgC shiftresult
          (shiftresult, sr') = computeShift src2 shft rf sr

compute' _ _ _ = error "compute' called on non-ALU instruction"

computeBic :: Bits a => a -> a -> a
computeBic x y = x .&. complement y

computeAdc :: Num a => StatusRegister -> a -> a -> a
computeAdc sr x y = x + y + if carry sr then 1 else 0

computeSbc :: Num a => StatusRegister -> a -> a -> a
computeSbc sr x y = x + y + if not $ carry sr then 1 else 0

computeBranch :: Argument a
              -> Condition
              -> RegisterFile
              -> StatusRegister
              -> (RegisterFile, StatusRegister)
computeBranch src cond rf sr =
    (branch,sr) where
    (branch,_) = computeArith (+) PC (ArgR PC) offset' cond sr rf setSRarith2
    offset     = src `eval` rf
    offset'    = computeBranchOffset (ArgC offset)

computeArith :: (Int32 -> Int32 -> Int32)
              -> Destination
              -> Argument a
              -> Argument b
              -> Condition
              -> StatusRegister
              -> RegisterFile
              -> (Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister)
              -> (RegisterFile, StatusRegister)

computeArith op dest src1 src2 cond sr rf srupdate =
    case checkCondition cond sr of
        False -> (rf, sr)
        True  -> (rf', sr') where
            a      = src1 `eval` rf
            b      = src2 `eval` rf
            result = a `op` b
            rf'    = set rf dest result
            sr'    = srupdate a b result sr

computeLongArith :: (Bits a, Num a, Integral a)
                 => (a -> a -> a)
                 -> Register
                 -> Register
                 -> Argument Register
                 -> Argument Register
                 -> Condition
                 -> StatusRegister
                 -> RegisterFile
                 -> (a -> StatusRegister -> StatusRegister)
                 -> (RegisterFile, StatusRegister)

computeLongArith op dest1 dest2 src1 src2 cond sr rf srupdate =
    if checkCondition cond sr then
        (rf'', sr')
    else
        (rf, sr)
    where a        = fromIntegral $ src1 `eval` rf
          b        = fromIntegral $ src2 `eval` rf
          result   = a `op` b
          result_t = fromIntegral $ result `shiftR` 32
          result_b = fromIntegral result
          rf'      = set rf dest1 result_b
          rf''     = set rf' dest2 result_t
          sr'      = srupdate result sr

computeLongArithAccum :: (Bits a, Num a, Integral a)
                      => (a -> a -> a)
                      -> Register
                      -> Register
                      -> Argument Register
                      -> Argument Register
                      -> Condition
                      -> StatusRegister
                      -> RegisterFile
                      -> (a -> StatusRegister -> StatusRegister)
                      -> (RegisterFile, StatusRegister)

computeLongArithAccum op dest1 dest2 src1 src2 cond sr rf srupdate =
    if checkCondition cond sr then
        (rf''', sr')
    else
        (rf, sr)
    where (rf',_)  = computeLongArith op dest1 dest2 src1 src2 cond sr rf srupdate
          rf''     = set rf dest1 result_b
          rf'''    = set rf'' dest2 result_t
          sr'      = srupdate result sr
          acc_b    = fromIntegral $ rf `get` dest1 :: Word64
          acc_t    = fromIntegral $ rf `get` dest2 :: Word64
          acc      = (acc_t `shiftL` 32) .|. acc_b
          val_b    = fromIntegral $ rf' `get` dest1 :: Word64
          val_t    = fromIntegral $ rf' `get` dest2 :: Word64
          val      = (val_t `shiftL` 32) .|. val_b
          result   = fromIntegral $ acc + val
          result_t = fromIntegral $ result `shiftR` 32 :: Int32
          result_b = fromIntegral result :: Int32

setSRarith1 :: Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister
setSRarith1 _ _ result sr = sr { zero = result == 0
                               , negative = result `testBit` 31
                               }

setSRarith2 :: Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister
setSRarith2 a b result sr = sr { carry    = isCarry (+) a b
                               , overflow = isOverflow a b
                               , zero = result == 0
                               , negative = result `testBit` 31
                               }

setSRarith3 :: Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister
setSRarith3 a b result sr = sr { carry    = not $ isCarry (-) a b
                               , overflow = isOverflow a (negate b)
                               , zero = result == 0
                               , negative = result `testBit` 31
                               }

setSRarithLong :: (Bits a, Num a, Integral a) => a -> StatusRegister -> StatusRegister
setSRarithLong result sr = sr { negative = result `testBit` 63
                              , zero     = result == 0
                              }

computeAddress :: AddressingModeMain
               -> RegisterFile
               -> StatusRegister
               -> (Word32,RegisterFile)

computeAddress (ImmPreIndex (ArgR rn) (ArgC imm) u o) rf _ =
    (fromIntegral result, rf')
    where result = a `op` b
          rf'    = case u of
                      Update -> set rf rn result
                      NoUpdate -> rf
          op     = getDirection o
          a      = rf `get` rn
          b      = fromIntegral imm

computeAddress (RegPreIndex (ArgR rn) (ArgR rm) shft u o) rf sr =
    (fromIntegral result, rf')
    where result = a `op` b
          rf'    = case u of
                      Update -> set rf rn result
                      NoUpdate -> rf
          op     = getDirection o
          a      = rf `get` rn
          (b,_)  = computeShift (ArgR rm) shft rf sr

computeAddress (ImmPostIndex (ArgR rn) (ArgC imm) o) rf _ =
    (fromIntegral result, rf')
    where result = rf `get` rn
          rf'    = set rf rn (result `op` b)
          b      = fromIntegral imm
          op     = getDirection o

computeAddress (RegPostIndex (ArgR rn) (ArgR rm) shft o) rf sr =
    (fromIntegral result, rf')
    where result = rf `get` rn
          rf'    = set rf rn (result `op` b)
          op     = getDirection o
          (b,_)  = computeShift (ArgR rm) shft rf sr

getDirection :: (Num a) => OffsetDir -> a -> a -> a

getDirection o = case o of Up   -> (+)
                           Down -> (-)

computeShift :: Argument a
             -> ShiftOp b
             -> RegisterFile
             -> StatusRegister
             -> (Int32, StatusRegister)

computeShift val NoShift rf sr = (val `eval` rf, sr)

computeShift val (RRX) rf sr = computeRotateR (val `eval` rf) (ArgC 0) rf sr

computeShift val (LSL shft) rf sr = computeShiftL (val `eval` rf) shft rf sr

computeShift val (ROR shft) rf sr = computeRotateR (val `eval` rf) shft rf sr

computeShift val (ASR shft) rf sr = computeShiftR (val `eval` rf) shft rf sr

computeShift val (LSR shft) rf sr = (fromIntegral result, sr')
    where (result, sr') = computeShiftR val' shft rf sr
          val' = fromIntegral (val `eval` rf) :: Word32


computeShiftL :: (Num a, Bits a)
              => a
              -> Argument b
              -> RegisterFile
              -> StatusRegister
              -> (a, StatusRegister)

computeShiftL val shft rf sr =
    (result, sr')
    where result = val `shiftL` degree
          degree = fromIntegral $ shft `eval` rf
          sr' | degree == 0 = sr
              | degree <= 32 = sr { carry = val `testBit` (32 - degree) }
              | otherwise = sr { carry = False }

computeShiftR :: (Num a, Bits a)
              => a
              -> Argument b
              -> RegisterFile
              -> StatusRegister
              -> (a, StatusRegister)

computeShiftR val (ArgC shft) _  sr =
    (result, sr')
    where result | degree == 0 = 0
                 | otherwise = val `shiftL` degree
          sr'    | degree == 0 = sr { carry = val `testBit` 31 }
                 | otherwise = sr { carry = val `testBit` (degree - 1) }
          degree = fromIntegral shft :: Int

computeShiftR val (ArgR shft) rf sr =
    (result, sr')
    where result = val `shiftR` degree
          degree = fromIntegral $ rf `get` shft
          sr' | degree == 0 = sr
              | degree <= 32 = sr { carry = val `testBit` (degree - 1) }
              | degree > 32 = sr { carry = False }

computeRotateR :: (Num a, Bits a)
               => a
               -> Argument b
               -> RegisterFile
               -> StatusRegister
               -> (a, StatusRegister)

computeRotateR val (ArgC shft) _ sr =
    case shft of
        0 -> (result, sr') where c | carry sr  = bit 31
                                   | otherwise = 0
                                 result        = c .|. (val `shiftR` 1)
                                 sr'           = sr { carry = val `testBit` 0 }
        _ -> (result, sr') where
             degree = fromIntegral shft :: Int
             result = val `rotateR` degree
             sr'    = sr { carry = val `testBit` (degree - 1) }

computeRotateR val (ArgR shft) rf sr =

    (result, sr')
    where result | degree  == 0 = val
                 | degree' >  0 = val `rotateR` degree'
          sr'    | degree  == 0 = sr
                 | degree' == 0 = sr { carry = val `testBit` 31 }
                 | degree' >  0 = sr { carry = val `testBit` (degree' - 1) }
          degree                = fromIntegral $ rf `get` shft
          degree'               = fromIntegral $ degree .&. bitmask 5

-- computeBranchOffset src := (SignExtend_30(signed_immed_24(src)) << 2)

-- This is semantically consistent with the ISA defined behaviour for arguments
-- to the B and BL instructions.
computeBranchOffset :: Argument Constant -> Argument Constant
computeBranchOffset (ArgC val) =
    ArgC result
    where result                    = se_src `shiftL` 2
          se_src | src `testBit` 23 = src .|. fromIntegral (complement mask)
                 | otherwise        = src
          src                       = fromIntegral mask .&. val
          mask                      = bitmask 24

checkCondition :: Condition -> StatusRegister -> Bool
checkCondition AL _    = True
checkCondition EQ cpsr = zero cpsr
checkCondition CS cpsr = carry cpsr
checkCondition MI cpsr = negative cpsr
checkCondition VS cpsr = overflow cpsr
checkCondition HI cpsr = carry cpsr && not (zero cpsr)
checkCondition GE cpsr = negative cpsr == overflow cpsr
checkCondition NE cpsr = not $ checkCondition EQ cpsr
checkCondition CC cpsr = not $ checkCondition CS cpsr
checkCondition PL cpsr = not $ checkCondition MI cpsr
checkCondition VC cpsr = not $ checkCondition VS cpsr
checkCondition LS cpsr = not $ checkCondition HI cpsr
checkCondition LT cpsr = not $ checkCondition GE cpsr
checkCondition GT cpsr = not (zero cpsr) && checkCondition GE cpsr
checkCondition LE cpsr = not $ checkCondition GT cpsr
checkCondition HS cpsr = checkCondition CS cpsr
checkCondition LO cpsr = checkCondition CC cpsr
