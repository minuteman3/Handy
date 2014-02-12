module Handy.ALU where

import Handy.Instructions
import Handy.StatusRegister
import Handy.Registers
import Handy.Util
import Data.Int  (Int32)
import Data.Word (Word32, Word64)
import Data.Bits

compute :: Instruction -> RegisterFile -> StatusRegister -> (RegisterFile, StatusRegister)

compute (MOV cond dest src shft) rf cpsr = computeArith1 id dest src cond cpsr rf
compute (MVN cond dest src shft) rf cpsr = computeArith1 complement dest src cond cpsr rf

compute (MUL cond dest src1 src2) rf cpsr = computeArith2 (*) dest src1 src2 cond cpsr rf

compute (ADD cond dest src1 src2 shft) rf cpsr = computeArith2 (+) dest src1 src2 cond cpsr rf

compute (SUB cond dest src1 src2 shft) rf cpsr = if (cond `checkCondition` cpsr)
                                                 then undefined
                                                 else (rf,cpsr)

compute (RSB cond dest src1 src2 shft) rf cpsr = if (cond `checkCondition` cpsr)
                                                 then undefined
                                                 else (rf,cpsr)
compute (AND cond dest src1 src2 shft) rf cpsr = if (cond `checkCondition` cpsr)
                                                 then undefined
                                                 else (rf,cpsr)
compute (ORR cond dest src1 src2 shft) rf cpsr = if (cond `checkCondition` cpsr)
                                                 then undefined
                                                 else (rf,cpsr)
compute (EOR cond dest src1 src2 shft) rf cpsr = if (cond `checkCondition` cpsr)
                                                 then undefined
                                                 else (rf,cpsr)

{-computeMov :: Int32 -> StatusRegister -> (Int32 -> Int32) -> (Int32, StatusRegister)-}
{-computeMov a cpsr op = (op a, cpsr')-}
                       {-where cpsr' = cpsr { zero = result == 0,-}
                                          {-, carry = result `testBit` 31-}
                                          {-}-}

{-computeAdd :: Int32 -> Int32 -> StatusRegister (Int32 -> Int32) -> (Int32, StatusRegister)-}
{-computeAdd a b cpsr op = (result, cpsr')-}
                         {-where result = a `op` b-}
                               {-cpsr'  = cpsr { zero = result == 0-}
                                             {-, negative = result `testBit` 31-}
                                             {-, carry = isCarry a b-}
                                             {-, overflow = isOverflow a b-}
                                             {-}-}
computeArith1 :: (Int32 -> Int32)
              -> Destination
              -> Argument a
              -> Condition
              -> StatusRegister
              -> RegisterFile
              -> (RegisterFile, StatusRegister)

computeArith1 op dest src cond sr rf = case checkCondition cond sr of
                                         False -> (rf, sr)
                                         True  -> (rf', sr') where
                                                a = src `eval` rf
                                                result = op a
                                                rf'    = set rf dest result
                                                sr'    = setSRarith1 result False sr

computeArith2 :: (Int32 -> Int32 -> Int32)
              -> Destination
              -> Argument a
              -> Argument b
              -> Condition
              -> StatusRegister
              -> RegisterFile
              -> (RegisterFile, StatusRegister)

computeArith2 op dest src1 src2 cond sr rf = case checkCondition cond sr of
                                             False -> (rf, sr)
                                             True  -> (rf', sr') where
                                                    a = src1 `eval` rf
                                                    b = src2 `eval` rf
                                                    result = a `op` b
                                                    rf'    = set rf dest result
                                                    sr'    = setSRarith2 a b result sr

setSRarith1 :: Int32 -> Bool -> StatusRegister -> StatusRegister
setSRarith1 result sc sr = sr { zero = result == 0
                              , negative = result `testBit` 31
                              , carry = sc
                              }

setSRarith2 :: Int32 -> Int32 -> Int32 -> StatusRegister -> StatusRegister
setSRarith2 a b result sr = sr { zero     = result == 0
                               , carry    = isCarry a b
                               , negative = result `testBit` 31
                               , overflow = isOverflow a b
                               }

{-computeSub a b cpsr = (result, cpsr')-}
                      {-where result = a - b-}
                            {-cpsr'  = cpsr { zero = result == 0-}
                                          {-, negative = result `testBit` 31-}
                                          {-, -}

{-compute1ary :: (Word64 -> Word64)-}
            {--> (StatusRegister -> StatusRegister)-}
            {--> Condition-}
            {--> RegisterFile-}
            {--> StatusRegister-}
            {--> (Int32, StatusRegister)-}

{-compute1ary op update cond rf cpsr = if cond `checkCondition` cpsr then-}

{-compute2ary :: (Word64 -> Word64 -> Word64)-}
            {--> (StatusRegister -> StatusRegister)-}
            {--> Condition-}
            {--> RegisterFile-}
            {--> StatusRegister-}
            {--> (Int32, RegisterFile, StatusRegister)-}

{-compute2ary op update cond rf cpsr = -}

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
