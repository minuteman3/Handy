module Handy.ALU where

import Handy.Instructions
import Handy.StatusRegister
import Handy.Registers
import Handy.Util
import Data.Int  (Int32)
import Data.Word (Word32, Word64)
import Data.Bits

compute :: Instruction -> RegisterFile -> StatusRegister -> (RegisterFile, StatusRegister)

compute (MOV cond dest src shft) rf cpsr = computeArith (const.id) dest src src cond cpsr rf setSRarith1
compute (MVN cond dest src shft) rf cpsr = computeArith (const.complement) dest src src cond cpsr rf setSRarith1
compute (MUL cond dest src1 src2) rf cpsr = computeArith (*) dest src1 src2 cond cpsr rf setSRarith1
compute (ADD cond dest src1 src2 shft) rf cpsr = computeArith (+) dest src1 src2 cond cpsr rf setSRarith2
compute (SUB cond dest src1 src2 shft) rf cpsr = computeArith (-) dest src1 src2 cond cpsr rf setSRarith3
compute (RSB cond dest src1 src2 shft) rf cpsr = computeArith (-) dest src2 src1 cond cpsr rf setSRarith3
compute (CMP cond src1 src2 shft) rf cpsr = computeArith (-) None src1 src2 cond cpsr rf setSRarith3
compute (AND cond dest src1 src2 shft) rf cpsr = computeArith (.&.) dest src1 src2 cond cpsr rf setSRarith1
compute (ORR cond dest src1 src2 shft) rf cpsr = computeArith (.|.) dest src1 src2 cond cpsr rf setSRarith1
compute (EOR cond dest src1 src2 shft) rf cpsr = computeArith (xor) dest src1 src2 cond cpsr rf setSRarith1

computeArith :: (Int32 -> Int32 -> Int32)
              -> Destination
              -> Argument a
              -> Argument b
              -> Condition
              -> StatusRegister
              -> RegisterFile
              -> (Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister)
              -> (RegisterFile, StatusRegister)
computeArith op dest src1 src2 cond sr rf srupdate = case checkCondition cond sr of
                                                        False -> (rf, sr)
                                                        True  -> (rf', sr') where
                                                            a = src1 `eval` rf
                                                            b = src2 `eval` rf
                                                            result = a `op` b
                                                            rf'    = set rf dest result
                                                            sr'    = srupdate a b result sr

setSRarith1 :: Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister
setSRarith1 _ _ result sr = sr { zero = result == 0
                               , negative = result `testBit` 31
                               }

setSRarith2 :: Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister
setSRarith2 a b result sr = sr { carry    = isCarry a b
                               , overflow = isOverflow a b
                               , zero = result == 0
                               , negative = result `testBit` 31
                               }

setSRarith3 :: Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister
setSRarith3 a b result sr = sr { carry    = isCarry a (negate b)
                               , overflow = isOverflow a (negate b)
                               , zero = result == 0
                               , negative = result `testBit` 31
                               }

computeShift :: Int32 -> ShiftOp a -> RegisterFile -> Int32
computeShift val (NoShift) _ = val
computeShift val (RRX) _ = undefined
computeShift val (LSL shft) rf = computeShift' shiftL val shft rf
computeShift val (ASR shft) rf = computeShift' shiftR val shft rf
computeShift val (ROR shft) rf = computeShift' rotateR val shft rf
computeShift val (LSR shft) rf = result where result  = fromIntegral $ val' `shiftR` degree'
                                              degree  = fromIntegral $ shft `eval` rf
                                              val'    = (fromIntegral val) :: Word32
                                              degree' = if degree == 0 then 32 else degree

computeShift' :: (Num a, Bits a) => (a -> Int -> a) -> a -> Argument b -> RegisterFile -> a
computeShift' op val shft rf = result where result = val `op` degree
                                            degree = fromIntegral $ shft `eval` rf
