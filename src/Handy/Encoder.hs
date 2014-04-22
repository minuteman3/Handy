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

serialiseUpdate :: UpdateReg -> Word32
serialiseUpdate u = fromIntegral (fromEnum u) `shiftL` 21

serialiseOffsetDir :: OffsetDir -> Word32
serialiseOffsetDir o = fromIntegral (fromEnum o) `shiftL` 23

serialiseAddressingMode :: AddressingModeMain -> Word32
serialiseAddressingMode (ImmPreIndex (ArgR rn) (ArgC imm) u o) =  serialiseReg 16 rn
                                                              .|. (fromIntegral imm .&. bitmask 12)
                                                              .|. serialiseUpdate u
                                                              .|. serialiseOffsetDir o
                                                              .|. bit 24

serialiseAddressingMode (RegPreIndex (ArgR rn) rm shft u o) =  serialiseReg 16 rn
                                                           .|. serialiseShift rm shft
                                                           .|. serialiseUpdate u
                                                           .|. serialiseOffsetDir o
                                                           .|. bit 24

serialiseAddressingMode (ImmPostIndex (ArgR rn) (ArgC imm) o) = serialiseReg 16 rn
                                                             .|. (fromIntegral imm .&. bitmask 12)
                                                             .|. serialiseOffsetDir o
serialiseAddressingMode (RegPostIndex (ArgR rn) rm shft o) = serialiseReg 16 rn
                                                          .|. serialiseShift rm shft
                                                          .|. serialiseOffsetDir o

serialiseAddresingModeMulti :: AddressingModeMulti -> Word32
serialiseAddresingModeMulti IA = bit 23
serialiseAddresingModeMulti IB = bit 24 .|. bit 23
serialiseAddresingModeMulti DA = 0
serialiseAddresingModeMulti DB = bit 24
serialiseAddresingModeMulti EA = serialiseAddresingModeMulti IA
serialiseAddresingModeMulti FA = serialiseAddresingModeMulti IB
serialiseAddresingModeMulti ED = serialiseAddresingModeMulti DA
serialiseAddresingModeMulti FD = serialiseAddresingModeMulti DB

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

serialiseRegList :: [Register] -> Word32
serialiseRegList = serialiseRegList' 0

serialiseRegList' :: Word32 -> [Register] -> Word32
serialiseRegList' val (None:regs) = serialiseRegList' val regs
serialiseRegList' val (PC:regs) = serialiseRegList' val (R15:regs)
serialiseRegList' val (LR:regs) = serialiseRegList' val (R14:regs)
serialiseRegList' val (SP:regs) = serialiseRegList' val (R13:regs)
serialiseRegList' val (rn:regs) = serialiseRegList' (val .|. bit (fromIntegral $ fromEnum rn)) regs
serialiseRegList' val []        = val


serialiseInstruction :: Instruction -> Word32
serialiseInstruction HALT                             = bitmask 32
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

serialiseInstruction (MUL cond s dest (ArgR src1) (ArgR src2))
    =  serialiseCondition cond
   .|. serialiseS s
   .|. serialiseReg 16 dest
   .|. serialiseReg 0  src1
   .|. serialiseReg 8  src2
   .|. bit 4
   .|. bit 7

serialiseInstruction (MLA cond s dest (ArgR src1) (ArgR src2) (ArgR src3))
    =  serialiseCondition cond
   .|. serialiseS s
   .|. serialiseReg 16 dest
   .|. serialiseReg 0  src1
   .|. serialiseReg 8  src2
   .|. serialiseReg 12 src3
   .|. bit 4
   .|. bit 7
   .|. bit 21

serialiseInstruction (UMULL cond s dest1 dest2 (ArgR src1) (ArgR src2))
    =  serialiseCondition cond
   .|. serialiseS s
   .|. serialiseReg 16 dest2
   .|. serialiseReg 12 dest1
   .|. serialiseReg 8  src2
   .|. serialiseReg 0 src1
   .|. bit 4
   .|. bit 7
   .|. bit 23

serialiseInstruction (SMULL cond s dest1 dest2 (ArgR src1) (ArgR src2))
    =  serialiseCondition cond
   .|. serialiseS s
   .|. serialiseReg 16 dest2
   .|. serialiseReg 12 dest1
   .|. serialiseReg 8  src2
   .|. serialiseReg 0 src1
   .|. bit 4
   .|. bit 7
   .|. bit 22
   .|. bit 23

serialiseInstruction (UMLAL cond s dest1 dest2 (ArgR src1) (ArgR src2))
    =  serialiseCondition cond
   .|. serialiseS s
   .|. serialiseReg 16 dest2
   .|. serialiseReg 12 dest1
   .|. serialiseReg 8  src2
   .|. serialiseReg 0 src1
   .|. bit 4
   .|. bit 7
   .|. bit 21
   .|. bit 23

serialiseInstruction (SMLAL cond s dest1 dest2 (ArgR src1) (ArgR src2))
    =  serialiseCondition cond
   .|. serialiseS s
   .|. serialiseReg 16 dest2
   .|. serialiseReg 12 dest1
   .|. serialiseReg 8  src2
   .|. serialiseReg 0 src1
   .|. bit 4
   .|. bit 7
   .|. bit 21
   .|. bit 22
   .|. bit 23

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

serialiseInstruction (LDR cond (ArgR src) addrm) = serialiseCondition cond
                                                .|. bit 26
                                                .|. bit 20
                                                .|. serialiseAddressingMode addrm
                                                .|. serialiseReg 12 src

serialiseInstruction (LDRB cond (ArgR src) addrm) = serialiseCondition cond
                                                 .|. bit 26
                                                 .|. bit 22
                                                 .|. bit 20
                                                 .|. serialiseAddressingMode addrm
                                                 .|. serialiseReg 12 src

serialiseInstruction (STR cond (ArgR src) addrm) = serialiseCondition cond
                                                .|. bit 26
                                                .|. serialiseAddressingMode addrm
                                                .|. serialiseReg 12 src

serialiseInstruction (STRB cond (ArgR src) addrm) = serialiseCondition cond
                                                 .|. bit 26
                                                 .|. bit 22
                                                 .|. serialiseAddressingMode addrm
                                                 .|. serialiseReg 12 src

serialiseInstruction (STM cond addrm (ArgR addr) update regs) =  serialiseCondition cond
                                                             .|. bit 27
                                                             .|. serialiseAddresingModeMulti addrm
                                                             .|. serialiseUpdate update
                                                             .|. serialiseReg 16 addr
                                                             .|. serialiseRegList regs

serialiseInstruction (LDM cond addrm addr update regs) =  serialiseInstruction (STM cond addrm addr update regs)
                                                      .|. bit 20
