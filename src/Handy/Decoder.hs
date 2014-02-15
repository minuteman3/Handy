{-# Language GADTs #-}
module Handy.Decoder where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as G
import Handy.Instructions
import Handy.Registers
import Handy.Util (bitmask)
import Data.Word (Word8, Word32)
import Data.Bits
import Control.Applicative


decode :: Maybe B.ByteString -> Maybe Instruction
decode Nothing   = Nothing
decode (Just iw) = Just $ G.runGet decode' iw

decode' :: G.Get Instruction
decode' = do condition <- G.lookAhead $ decodeCondition
             case condition of
               NV -> return HALT
               _  -> do instruction_type <- G.lookAhead $ decodeInstructionType
                        case instruction_type of
                           0 -> decodeType0 condition     -- Data processing immediate shift OR
                                                           -- Miscellaneous Instruction OR
                                                           -- Data processing register shift OR
                                                           -- Multiplies OR
                                                           -- Extra Load Stores OR
 
                           1 -> decodeType1 condition     -- Data processing immediate value OR
                                                           -- Undefined instruction OR
                                                           -- Move immediate to status register
 
                           2 -> do return JunkInstruction -- Load/store immediate offset
 
                           3 -> do return JunkInstruction -- Load/store register offset OR
                                                           -- Media instruction OR
                                                           -- Architecturally undefined
 
                           4 -> do return JunkInstruction -- Load/store multiple
 
                           5 -> decodeType5 condition     -- Branch/Branch Link
 
                           6 -> do return JunkInstruction -- Coprocessor load/store and double register transfers

                           7 -> do return JunkInstruction -- Coprocessor data processing OR
                                                           -- Coprocessor register transfers OR
                                                           -- Software interrupt
                           _ -> fail "Invalid instruction type"

decodeType0 :: Condition -> G.Get Instruction
decodeType0 cond = (decodeDataOp cond decodeDataShiftImm)
                <|> (decodeMul cond)
                <|> (decodeMisc cond)
                <|> (decodeDataOp cond decodeDataShiftReg)
                <|> empty


mulMask :: Word32
mulMask = bit 27 .|. bit 26 .|. bit 25 .|. bit 24 .|. bit 23 .|. bit 22 .|. bit 7 .|. bit 4

isMul :: Word32 -> Bool
isMul word = (word .&. mulMask) == (bit 7 .|. bit 4)

decodeMul :: Condition -> G.Get Instruction
decodeMul cond = do word <- G.lookAhead $ G.getWord32be
                    if isMul word then do
                        s <- G.lookAhead $ decodeS
                        let src1 = getRegister word 0
                            src2 = getRegister word 8
                            (ArgR dest) = getRegister word 16
                        return $ MUL cond s dest src1 src2
                    else
                        empty



decodeDataOp :: (ArgVal a, ArgVal b, Arg a, Arg b) => Condition -> G.Get (Argument a, ShiftOp b) -> G.Get Instruction
decodeDataOp cond parser = do (src2, shft) <- G.lookAhead $ parser
                              s <- G.lookAhead $ decodeS
                              opcode <- G.lookAhead $ getOpcode
                              src1 <- G.lookAhead $ decodeRegisterSrc
                              dest <- G.lookAhead $ decodeRegisterDest
                              makeInstruction opcode cond s dest src1 src2 shft



decodeMisc :: Condition -> G.Get Instruction
decodeMisc cond = do word <- G.lookAhead $ G.getWord32be
                     if word `testBit` 24 && word `testBit` 21 && word `testBit` 4 then do
                         let dest = getRegister word 0
                         return $ BX cond dest
                     else empty




decodeType1 :: Condition -> G.Get Instruction
decodeType1 cond = (decodeDataImmediate cond) <|> decodeMSR <|> decodeUndefined <|> empty

opcodeMask :: Word32
opcodeMask = (bitmask 4) `shiftL` 21

getOpcode :: G.Get Word8
getOpcode = do word <- G.getWord32be
               let opcode = word .&. opcodeMask
               return $ fromIntegral $ (opcode `shiftR` 21)

decodeS :: G.Get S
decodeS = do word <- G.getWord32be
             case word `testBit` 20 of
                        True  -> return S
                        False -> return NoS

decodeDataImmediate :: Condition -> G.Get Instruction
decodeDataImmediate cond = do (src2, shft) <- G.lookAhead decodeLiteral
                              s <- G.lookAhead $ decodeS
                              opcode <- G.lookAhead $ getOpcode
                              src1 <- G.lookAhead $ decodeRegisterSrc
                              dest <- G.lookAhead $ decodeRegisterDest
                              makeInstruction opcode cond s dest src1 src2 shft

makeInstruction :: Word8
                -> Condition
                -> S
                -> Destination
                -> Argument Register
                -> Argument a
                -> ShiftOp b
                -> G.Get Instruction

makeInstruction opcode cond s dest src1 src2 shft = case opcode of
                                0 -> return $ AND cond s dest src1 src2 shft
                                1 -> return $ EOR cond s dest src1 src2 shft
                                2 -> return $ SUB cond s dest src1 src2 shft
                                3 -> return $ RSB cond s dest src1 src2 shft
                                4 -> return $ ADD cond s dest src1 src2 shft
                                5 -> return $ ADC cond s dest src1 src2 shft
                                6 -> return $ SBC cond s dest src1 src2 shft
                                7 -> return $ RSC cond s dest src1 src2 shft
                                8 -> case s of
                                       S -> return $ TST cond src1 src2 shft
                                       _ -> empty
                                9 -> case s of
                                       S -> return $ TEQ cond src1 src2 shft
                                       _ -> empty
                                10 -> case s of
                                       S -> return $ CMP cond src1 src2 shft
                                       _ -> empty
                                11 -> case s of
                                       S -> return $ CMN cond src1 src2 shft
                                       _ -> empty
                                12 -> return $ ORR cond s dest src1 src2 shft
                                13 -> return $ MOV cond s dest src2 shft
                                14 -> return $ BIC cond s dest src1 src2 shft
                                15 -> return $ MVN cond s dest src2 shft
                                _  -> fail "Invalid opcode"

decodeMSR :: G.Get Instruction
decodeMSR = empty

decodeUndefined :: G.Get Instruction
decodeUndefined = empty

decodeType5 :: Condition -> G.Get Instruction
decodeType5 cond = do word <- G.getWord32be
                      let l = word `testBit` 24
                          offset = toArgument $ fromIntegral $ word .&. bitmask 24
                      if l then return $ BL cond offset else return $ B cond offset

getRegister :: Word32 -> Int -> Argument Register
getRegister word at = toArgument $ toEnum $ fromIntegral $ (word `shiftR` at) .&. bitmask 4

decodeRegisterDest :: G.Get Register
decodeRegisterDest = do word <- G.getWord32be
                        let (ArgR dest) = getRegister word 12
                        return dest

decodeRegisterSrc :: G.Get (Argument Register)
decodeRegisterSrc = do word <- G.getWord32be
                       return $ getRegister word 16

decodeCondition :: G.Get Condition
decodeCondition = do byte <- G.getWord8
                     let cond = toEnum $ fromIntegral $ byte `shiftR` 4
                     return cond

decodeInstructionType :: G.Get Word8
decodeInstructionType = do byte <- G.getWord8
                           let instruction_type = byte `shiftR` 1
                           return $ instruction_type .&. (fromIntegral $ bitmask 3)

decodeDataLiteral :: G.Get (Argument Constant, ShiftOp a)
decodeDataLiteral = do word <- G.lookAhead $ G.getWord32be
                       if isDataProc word then decodeLiteral <|> empty else empty

decodeDataShiftImm :: G.Get (Argument Register, ShiftOp Constant)
decodeDataShiftImm = do word <- G.lookAhead $ G.getWord32be
                        if word `testBit` 4 then
                            empty
                        else
                            decodeShiftLImm <|> decodeShiftRImm <|> decodeShiftAImm <|> decodeRotateImm <|> empty

decodeDataShiftReg :: G.Get (Argument Register, ShiftOp Register)
decodeDataShiftReg = do word <- G.lookAhead $ G.getWord32be
                        if (not $ word `testBit` 4) || word `testBit` 7 || (not $ isDataProc word) then
                            empty
                        else
                            decodeShiftLReg <|> decodeShiftRReg <|> decodeShiftAReg <|> decodeRotateReg <|> empty

dataProcMask :: Word32
dataProcMask = bit 24 .|. bit 23 .|. bit 20

isDataProc :: Word32 -> Bool
isDataProc word = (word .&. dataProcMask) /= 0

shiftMask :: Word32
shiftMask = bit 6 .|. bit 5

isShiftL :: Word32 -> Bool
isShiftL word = (word .&. shiftMask) == 0

isLShiftR :: Word32 -> Bool
isLShiftR word = (word .&. shiftMask) == bit 5

isAShiftR :: Word32 -> Bool
isAShiftR word = (word .&. shiftMask) == bit 6

isRotateR :: Word32 -> Bool
isRotateR word =(word .&. shiftMask) == shiftMask

evaluateImm :: Word32 -> Word32
evaluateImm i = result where shft = i `shiftR` 8
                             val  = i .&. bitmask 8
                             result = val `rotateR` (fromIntegral $ shft * 2)

decodeLiteral :: G.Get (Argument Constant, ShiftOp a)
decodeLiteral = do word <- G.getWord32be
                   if word `testBit` 25 then do
                      let imm_part = word .&. bitmask 12
                          immediate = fromIntegral $ evaluateImm imm_part
                      return $ (toArgument $ immediate, NoShift)
                   else empty


decodeShiftLReg :: G.Get (Argument Register, ShiftOp Register)
decodeShiftLReg = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isShiftL word then
                        do let register = word `getRegister` 0
                           let shift_reg = word `getRegister` 8
                           return $ (register, LSL shift_reg)
                     else empty


decodeShiftRReg :: G.Get (Argument Register, ShiftOp Register)
decodeShiftRReg = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isLShiftR word then
                        do let register = word `getRegister` 0
                           let shift_reg = word `getRegister` 8
                           return $ (register, LSR shift_reg)
                     else empty

decodeShiftAReg :: G.Get (Argument Register, ShiftOp Register)
decodeShiftAReg = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isAShiftR word then
                        do let register = word `getRegister` 0
                           let shift_reg = word `getRegister` 8
                           return $ (register, ASR shift_reg)
                     else empty

decodeRotateReg :: G.Get (Argument Register, ShiftOp Register)
decodeRotateReg = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isRotateR word then
                        do let register = word `getRegister` 0
                           let shift_reg = word `getRegister` 8
                           return $ (register, ROR shift_reg)
                     else empty

decodeShiftLImm :: G.Get (Argument Register, ShiftOp Constant)
decodeShiftLImm = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isShiftL word then
                          do let register = word `getRegister` 0
                             let shft = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                             return $ (register, LSL shft)
                     else empty

decodeShiftRImm :: G.Get (Argument Register, ShiftOp Constant)
decodeShiftRImm = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isLShiftR word then
                        do let register = word `getRegister` 0
                           let shft = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                           return $ (register, LSR shft)
                     else empty

decodeShiftAImm :: G.Get (Argument Register, ShiftOp Constant)
decodeShiftAImm = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isAShiftR word then
                        do let register = word `getRegister` 0
                           let shft = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                           return $ (register, ASR shft)
                     else empty

decodeRotateImm :: G.Get (Argument Register, ShiftOp Constant)
decodeRotateImm = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if isRotateR word then
                       do let register = word `getRegister` 0
                          let shft = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                          case shft of
                            ArgC 0 -> return $ (register, RRX)
                            _ -> return $ (register, ROR shft)
                     else empty
