{-# Language GADTs #-}
module Handy.Decoder where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as G
import Handy.Instructions
import Handy.Registers
import Handy.Util (bitmask)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int32)
import Data.Bits
import Control.Applicative
import System.IO.Unsafe


decode :: G.Get Instruction
decode = do condition <- G.lookAhead $ decodeCondition
            instruction_type <- G.lookAhead $ decodeInstructionType
            case instruction_type of
              0 -> do return JunkInstruction -- Data processing immediate shift OR
                                             -- Miscellaneous Instruction OR
                                             -- Data processing register shift

              1 -> do return JunkInstruction -- Data processing immediate value OR
                                             -- Undefined instruction OR
                                             -- Move immediate to status register

              2 -> do return JunkInstruction -- Load/store immediate offset

              3 -> decodeType3 condition     -- Load/store register offset OR
                                             -- Media instruction OR
                                             -- Architecturally undefined

              4 -> do return JunkInstruction -- Load/store multiple

              5 -> decodeType5 condition     -- Branch/Branch Link

              6 -> do return JunkInstruction -- Coprocessor load/store and double register transfers

              7 -> do return JunkInstruction -- Coprocessor data processing OR
                                             -- Coprocessor register transfers OR
                                             -- Software interrupt

decodeType3 :: Condition -> G.Get Instruction
decodeType3 cond = (decodeDataImmediate cond) <|> decodeMSR <|> decodeUndefined

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
decodeDataImmediate cond = do (src2, NoShift) <- G.lookAhead decodeLiteral
                              s <- G.lookAhead $ decodeS
                              opcode <- G.lookAhead $ getOpcode
                              src1 <- G.lookAhead $ decodeRegisterSrc
                              dest <- G.lookAhead $ decodeRegisterDest
                              case opcode of
                                0 -> return $ AND cond s dest src1 src2 NoShift
                                1 -> return $ EOR cond s dest src1 src2 NoShift
                                2 -> return $ SUB cond s dest src1 src2 NoShift
                                3 -> return $ RSB cond s dest src1 src2 NoShift
                                4 -> return $ ADD cond s dest src1 src2 NoShift
                                5 -> return $ JunkInstruction -- ADC cond s dest src1 src2 NoShift
                                6 -> return $ JunkInstruction -- SBC cond s dest src1 src2 NoShift
                                7 -> return $ JunkInstruction -- RSC cond s dest src1 src2 NoShift
                                8 -> case s of
                                       S -> return $ JunkInstruction -- TST cond src1 src2 NoShift
                                       _ -> empty
                                9 -> case s of
                                       S -> return $ JunkInstruction -- TEQ cond src1 src2 NoShift
                                       _ -> empty
                                10 -> case s of
                                       S -> return $ CMP cond src1 src2 NoShift
                                       _ -> empty
                                11 -> case s of
                                       S -> return $ JunkInstruction -- CMN cond src1 src2 NoShift
                                       _ -> empty
                                12 -> return $ ORR cond s dest src1 src2 NoShift
                                13 -> return $ MOV cond s dest src2 NoShift
                                14 -> return $ JunkInstruction -- BIC cond s dest src1 src2 NoShift
                                15 -> return $ MVN cond s dest src2 NoShift

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

decodeDataProcessing = decodeLiteral <|> empty

decodeDataShiftImm = decodeShiftImm <|> decodeShiftRImm <|> empty
decodeDataShiftReg = decodeShiftLReg <|> decodeShiftRReg <|> empty

decodeLiteral :: G.Get (Argument Constant, ShiftOp a)
decodeLiteral = do word <- G.getWord32be
                   if word `testBit` 25 then do
                      let immed_8 = word .&. bitmask 8
                      let rotate_imm = fromIntegral $ (word `shiftR` 8) .&. bitmask 4
                      let immediate = (fromIntegral $ immed_8 `rotateR` rotate_imm) :: Int32
                      return $ (toArgument $ immediate, NoShift)
                   else empty

decodeShiftImm :: G.Get (Argument Register, ShiftOp Constant)
decodeShiftImm = do word <- G.getWord32be
                    if word `testBit` 25 then empty
                    else if word `testBit` 4 then empty
                    else do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                            let shift = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                            return $ (register, LSL shift)

decodeShiftLReg :: G.Get (Argument Register, ShiftOp Register)
decodeShiftLReg = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if word `testBit` 4 then
                        do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                           let shift = (word `shiftR` 8) .&. bitmask 4
                           let shift_reg = toArgument $ toEnum $ fromIntegral $ shift
                           return $ (register, LSL shift_reg)
                     else empty

decodeShiftRImm :: G.Get (Argument Register, ShiftOp Constant)
decodeShiftRImm = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if word `testBit` 5 then
                        do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                           let shift = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                           return $ (register, LSR shift)
                     else empty

decodeShiftRReg :: G.Get (Argument Register, ShiftOp Register)
decodeShiftRReg = do word <- G.getWord32be
                     if word `testBit` 25 then empty
                     else if word `testBit` 5 && word `testBit` 4 then
                        do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                           let shift = (word `shiftR` 8) .&. bitmask 4
                           let shift_reg = toArgument $ toEnum $ fromIntegral $ shift
                           return $ (register, LSR shift_reg)
                     else empty

decodeShiftAImm :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Constant))
decodeShiftAImm word = if word `testBit` 25 then return Nothing
                       else if word `testBit` 6 then
                          do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                             let shift = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                             return $ Just (register, ASR shift)
                       else return Nothing

decodeShiftAReg :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Register))
decodeShiftAReg word = if word `testBit` 25 then return Nothing
                       else if word `testBit` 6 && word `testBit` 4 then
                          do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                             let shift = (word `shiftR` 8) .&. bitmask 4
                             let shift_reg = toArgument $ toEnum $ fromIntegral $ shift
                             return $ Just (register, ASR shift_reg)
                       else return Nothing

decodeRotateImm :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Constant))
decodeRotateImm word = if word `testBit` 25 then return Nothing
                       else if word `testBit` 6 && word `testBit` 5 then
                         do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                            let shift = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                            case shift of
                              ArgC 0 -> return $ Just (register, RRX)
                              _ -> return $ Just (register, ROR shift)
                       else return Nothing

decodeRotateReg :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Constant))
decodeRotateReg word = if word `testBit` 25 then return Nothing
                       else if word `testBit` 6 && word `testBit` 5 && word `testBit` 4 then
                          do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                             let shift = (word `shiftR` 8) .&. bitmask 4
                             let shift_reg = toArgument $ toEnum $ fromIntegral $ shift
                             return $ Just (register, ROR shift_reg)
                       else return Nothing
