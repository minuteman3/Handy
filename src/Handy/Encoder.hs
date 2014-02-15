{-# Language GADTs #-}
module Handy.Encoder where
import Handy.Instructions
import Handy.Registers
import Handy.Util (bitmask)
import Data.Word (Word8,Word32)
import Data.Int (Int32)
import Data.Bits

data Opcode = OpAND
            | OpEOR
            | OpSUB
            | OpRSB
            | OpADD
            | OpADC
            | OpSBC
            | OpRSC
            | OpTST
            | OpTEQ
            | OpCMP
            | OpCMN
            | OpORR
            | OpMOV
            | OpBIC
            | OpMVN
            deriving (Eq,Show,Enum,Ord)

makeImm :: Word32 -> (Word8,Word8)
makeImm val = if val > 255 then makeImm' 0 val val else (fromIntegral val,0)


makeImm' :: Int -> Word32 -> Word32 -> (Word8,Word8)
makeImm' i val orig | popCount orig > 8 = error "Invalid immediate value"
                    | i > 30 = error "Invalid immediate value"
                    | validImm orig val i = (fromIntegral val, fromIntegral i)
                    | otherwise = makeImm' (i+1) (val `rotateL` 1) orig

validImm :: Word32 -> Word32 -> Int -> Bool
validImm orig val i = i `mod` 2 == 0 && popCount  (val .&. bitmask 8) == popCount orig

serialiseImm :: Int32 -> Word32
serialiseImm i = result where (val,shft) = makeImm $ fromIntegral i
                              shft'      = fromIntegral (shft `shiftR` 1) :: Word32
                              shift_part = shft' `shiftL` 8
                              val_part   = fromIntegral val
                              result     = shift_part .|. val_part .|. bit 25

serialiseCondition :: Condition -> Word32
serialiseCondition c = (fromIntegral . fromEnum $ c) `rotateR` 4

serialiseRegShftImm :: Register -> Constant -> Word32
serialiseRegShftImm reg shft = serialiseReg 0 reg .|. serialiseShiftConstant shft

serialiseRegShftReg :: Register -> Register -> Word32
serialiseRegShftReg reg shft = serialiseReg 0 reg .|. serialiseReg 8 shft .|. bit 4

serialiseShift :: Argument a -> ShiftOp b -> Word32
serialiseShift (ArgC val) NoShift = serialiseImm val
serialiseShift (ArgR reg) NoShift = serialiseReg 0 reg
serialiseShift (ArgR reg) (LSL (ArgC shft)) = serialiseRegShftImm reg shft .|. serialiseLSL
serialiseShift (ArgR reg) (LSR (ArgC shft)) = serialiseRegShftImm reg shft .|. serialiseLSR
serialiseShift (ArgR reg) (ASR (ArgC shft)) = serialiseRegShftImm reg shft .|. serialiseASR
serialiseShift (ArgR reg) (ROR (ArgC shft)) = serialiseRegShftImm reg shft .|. serialiseROR
serialiseShift (ArgR reg) RRX               = serialiseRegShftImm reg 0    .|. serialiseROR
serialiseShift (ArgR reg) (LSL (ArgR shft)) = serialiseRegShftReg reg shft .|. serialiseLSL
serialiseShift (ArgR reg) (LSR (ArgR shft)) = serialiseRegShftReg reg shft .|. serialiseLSR
serialiseShift (ArgR reg) (ASR (ArgR shft)) = serialiseRegShftReg reg shft .|. serialiseASR
serialiseShift (ArgR reg) (ROR (ArgR shft)) = serialiseRegShftReg reg shft .|. serialiseROR

serialiseShiftConstant :: Int32 -> Word32
serialiseShiftConstant c = fromIntegral ((c .&. fromIntegral (bitmask 5)) `shiftL` 7)

serialiseLSL :: Word32
serialiseLSR :: Word32
serialiseASR :: Word32
serialiseROR :: Word32

serialiseLSL = 0
serialiseLSR = bit 5
serialiseASR = bit 6
serialiseROR = bit 6 .|. bit 5

serialiseOpcode :: Opcode -> Word32
serialiseOpcode op = (fromIntegral . fromEnum $ op) `shiftL` 21

serialiseS :: S -> Word32
serialiseS s = (fromIntegral . fromEnum $ s) `shiftL` 20

serialiseReg :: Int -> Register -> Word32
serialiseReg place r  = (fromIntegral . fromEnum $ r) `shiftL` place

serialiseCompareInstruction :: Condition -> Argument Register -> Argument a -> ShiftOp b -> Word32
serialiseCompareInstruction cond (ArgR reg1) src2 shft =  serialiseCondition cond
                                               .|. serialiseS S
                                               .|. serialiseReg 16 reg1
                                               .|. serialiseShift src2 shft

serialise1aryInstruction' :: Condition -> S -> Destination -> Argument a -> ShiftOp b -> Word32
serialise1aryInstruction' cond s dest src shft =  serialiseCondition cond
                                              .|. serialiseS s
                                              .|. serialiseReg 12 dest
                                              .|. serialiseShift src shft

serialise2aryInstruction' :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Word32
serialise2aryInstruction' cond s dest (ArgR reg1) src2 shft =  serialiseCondition cond
                                                           .|. serialiseS s
                                                           .|. serialiseReg 12 dest
                                                           .|. serialiseReg 16 reg1
                                                           .|. serialiseShift src2 shft


serialiseInstruction :: Instruction -> Word32
serialiseInstruction HALT                             = 2^32 - 1
serialiseInstruction (ADD cond s dest src1 src2 shft) =  serialiseOpcode OpADD
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (AND cond s dest src1 src2 shft) =  serialiseOpcode OpAND
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (ADC cond s dest src1 src2 shft) =  serialiseOpcode OpADC
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (SUB cond s dest src1 src2 shft) =  serialiseOpcode OpSUB
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (RSB cond s dest src1 src2 shft) =  serialiseOpcode OpRSB
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (SBC cond s dest src1 src2 shft) =  serialiseOpcode OpSBC
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (RSC cond s dest src1 src2 shft) =  serialiseOpcode OpRSC
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (EOR cond s dest src1 src2 shft) =  serialiseOpcode OpEOR
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (ORR cond s dest src1 src2 shft) =  serialiseOpcode OpORR
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (BIC cond s dest src1 src2 shft) =  serialiseOpcode OpBIC
                                                     .|. serialise2aryInstruction' cond s dest src1 src2 shft
serialiseInstruction (MOV cond s dest src shft)       =  serialiseOpcode OpMOV
                                                     .|. serialise1aryInstruction' cond s dest src shft
serialiseInstruction (MVN cond s dest src shft)       =  serialiseOpcode OpMVN
                                                     .|. serialise1aryInstruction' cond s dest src shft
serialiseInstruction (CMP cond src1 src2 shft)        =  serialiseOpcode OpCMP
                                                     .|. serialiseCompareInstruction cond src1 src2 shft
serialiseInstruction (CMN cond src1 src2 shft)        =  serialiseOpcode OpCMN
                                                     .|. serialiseCompareInstruction cond src1 src2 shft
serialiseInstruction (TEQ cond src1 src2 shft)        =  serialiseOpcode OpTEQ
                                                     .|. serialiseCompareInstruction cond src1 src2 shft
serialiseInstruction (TST cond src1 src2 shft)        =  serialiseOpcode OpTST
                                                     .|. serialiseCompareInstruction cond src1 src2 shft

serialiseInstruction (MUL cond s dest (ArgR src1) (ArgR src2)) =  serialiseCondition cond
                                                              .|. serialiseS s
                                                              .|. serialiseReg 16 dest
                                                              .|. serialiseReg 8  src1
                                                              .|. serialiseReg 0  src2
                                                              .|. bit 4
                                                              .|. bit 7

serialiseInstruction (MLA cond s dest (ArgR src1) (ArgR src2) (ArgR src3)) =  serialiseCondition cond
                                                                          .|. serialiseS s
                                                                          .|. serialiseReg 16 dest
                                                                          .|. serialiseReg 8  src1
                                                                          .|. serialiseReg 0  src2
                                                                          .|. serialiseReg 12 src3
                                                                          .|. bit 4
                                                                          .|. bit 7
                                                                          .|. bit 21

serialiseInstruction (B cond (ArgC dest)) = serialiseCondition cond
                                         .|. bit 27
                                         .|. bit 25
                                         .|. (fromIntegral dest .&. bitmask 24)

serialiseInstruction (BL cond (ArgC dest)) = serialiseCondition cond
                                          .|. bit 27
                                          .|. bit 25
                                          .|. bit 24
                                          .|. (fromIntegral dest .&. bitmask 24)

serialiseInstruction (BX cond (ArgR dest)) =  serialiseCondition cond
                                          .|. bit 24
                                          .|. bit 21
                                          .|. bit 4
                                          .|. serialiseReg 0 dest
