{-# Language GADTs #-}
module Handy.Encoder (serialiseInstruction) where
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
makeImm' i val orig = if popCount orig > 8 then error "Invalid immediate value"
                      else if i > 30 then error "Invalid immediate value"
                      else if validImm orig val i then (fromIntegral val, fromIntegral i)
                      else makeImm' (i+1) (val `rotateL` 1) orig

validImm orig val i = i `mod` 2 == 0 && (popCount $ val .&. bitmask 8) == popCount orig

serialiseImm :: Int32 -> Word32
serialiseImm i = result where (val,shft) = makeImm $ fromIntegral i
                              shft'      = fromIntegral (shft `shiftR` 1) :: Word32
                              shift_part = shft' `shiftL` 8
                              val_part   = fromIntegral $ val
                              result     = shift_part .|. val_part .|. bit 25

serialiseCondition :: Condition -> Word32
serialiseCondition c = (fromIntegral $ fromEnum $ c) `rotateR` 4

serialiseRegShftImm :: Register -> Constant -> Word32
serialiseRegShftImm reg shft = (serialiseReg 0 reg) .|. (serialiseShiftConstant shft)

serialiseRegShftReg :: Register -> Register -> Word32
serialiseRegShftReg reg shft = (serialiseReg 0 reg) .|. (serialiseReg 8 reg) .|. bit 4

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
serialiseShiftConstant c = fromIntegral $ ((c .&. (fromIntegral $ bitmask 5)) `shiftL` 7)

serialiseLSL :: Word32
serialiseLSR :: Word32
serialiseASR :: Word32
serialiseROR :: Word32

serialiseLSL = 0
serialiseLSR = bit 5
serialiseASR = bit 6
serialiseROR = bit 6 .|. bit 5

serialiseOpcode :: Opcode -> Word32
serialiseOpcode op = (fromIntegral $ fromEnum $ op) `shiftL` 21

serialiseS :: S -> Word32
serialiseS s = (fromIntegral $ fromEnum $ s) `shiftL` 20

serialiseReg :: Int -> Register -> Word32
serialiseReg place r  = (fromIntegral $ fromEnum $ r) `shiftL` place

serialiseRegDest = serialiseReg 12
serialiseRegSrc1 = serialiseReg 16
serialiseRegSrc2 = serialiseReg 0

serialiseInstruction' :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Word32
serialiseInstruction' cond s dest (ArgR reg1) src2 shft =  serialiseCondition cond
                                                       .|. serialiseS s
                                                       .|. serialiseRegDest dest
                                                       .|. serialiseRegSrc1 reg1
                                                       .|. serialiseShift src2 shft


serialiseInstruction :: Instruction -> Word32
serialiseInstruction HALT                             = 2^32 - 1
serialiseInstruction (ADD cond s dest src1 src2 shft) =  serialiseOpcode OpADD
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (AND cond s dest src1 src2 shft) =  serialiseOpcode OpAND
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (ADC cond s dest src1 src2 shft) =  serialiseOpcode OpADC
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (SUB cond s dest src1 src2 shft) =  serialiseOpcode OpSUB
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (RSB cond s dest src1 src2 shft) =  serialiseOpcode OpRSB
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (SBC cond s dest src1 src2 shft) =  serialiseOpcode OpSBC
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (RSC cond s dest src1 src2 shft) =  serialiseOpcode OpRSC
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (EOR cond s dest src1 src2 shft) =  serialiseOpcode OpEOR
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
serialiseInstruction (ORR cond s dest src1 src2 shft) =  serialiseOpcode OpORR
                                                     .|. serialiseInstruction' cond s dest src1 src2 shft
