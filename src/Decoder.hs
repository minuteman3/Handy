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

decode :: G.Get (Condition,Word8)
decode = do condition <- G.lookAhead $ decodeCondition
            instruction_type <- G.lookAhead $ decodeInstructionType
            case instruction_type of
              0 -> do return () -- Data processing immediate shift OR
                                -- Miscellaneous Instruction OR
                                -- Data processing register shift

              1 -> do return () -- Data processing immediate value OR
                                -- Undefined instruction OR
                                -- Move immediate to status register

              2 -> do return () -- Load/store immediate offset

              3 -> do return () -- Load/store register offset OR
                                -- Media instruction OR
                                -- Architecturally undefined

              4 -> do return () -- Load/store multiple

              5 -> do return () -- Branch/Branch Link

              6 -> do return () -- Coprocessor load/store and double register transfers

              7 -> do return () -- Coprocessor data processing OR
                                -- Coprocessor register transfers OR
                                -- Software interrupt
            return (condition,instruction_type)

{-decodeOpcode :: Word32 -> -}
{-decodeS :: Word32 -> Maybe S-}
{-decodeS word = if word `testBit` 20 then Just S else Just NoS-}

{-getRegister :: Word32 -> Int -> Argument Register-}
{-getRegister word at = toArgument $ toEnum $ fromIntegral $ (word `shiftR` at) .&. bitmask 4-}

{-decodeRegisterDest :: Word32 -> Maybe (Argument Register)-}
{-decodeRegisterDest word = Just $ getRegister word 16-}

{-decodeRegisterSrc :: Word32 -> Maybe (Argument Register)-}
{-decodeRegisterSrc word = Just $ getRegister word 16-}

decodeCondition :: G.Get Condition
decodeCondition = do byte <- G.getWord8
                     let cond = toEnum $ fromIntegral $ byte `shiftR` 4
                     return cond

decodeInstructionType :: G.Get Word8
decodeInstructionType = do byte <- G.getWord8
                           let instruction_type = byte `shiftR` 1
                           return $ instruction_type .&. (fromIntegral $ bitmask 3)

decodeLiteral :: Word32 -> G.Get (Maybe (Argument Constant, ShiftOp a))
decodeLiteral word = do if word `testBit` 25 then do
                            let immed_8 = word .&. bitmask 8
                            let rotate_imm = fromIntegral $ (word `shiftR` 8) .&. bitmask 4
                            let immediate = (fromIntegral $ immed_8 `rotateR` rotate_imm) :: Int32
                            return $ Just (toArgument $ immediate, NoShift)
                        else return Nothing

decodeShiftImm :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Constant))
decodeShiftImm word = if word `testBit` 25 then
                         return Nothing
                      else if word `testBit` 4 then
                         return Nothing
                      else do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                              let shift = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                              return $ Just (register, LSL shift)

decodeShiftLReg :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Register))
decodeShiftLReg word = if word `testBit` 25 then
                          return Nothing
                       else if word `testBit` 4 then
                          do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                             let shift = (word `shiftR` 8) .&. bitmask 4
                             let shift_reg = toArgument $ toEnum $ fromIntegral $ shift
                             return $ Just (register, LSL shift_reg)
                       else return Nothing

decodeShiftRImm :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Constant))
decodeShiftRImm word = if word `testBit` 25 then return Nothing
                       else if word `testBit` 5 then
                          do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                             let shift = toArgument $ fromIntegral $ (word `shiftR` 7) .&. bitmask 5
                             return $ Just (register, LSR shift)
                       else return Nothing

decodeShiftRReg :: Word32 -> G.Get (Maybe (Argument Register, ShiftOp Register))
decodeShiftRReg word = if word `testBit` 25 then return Nothing
                       else if word `testBit` 5 && word `testBit` 4 then
                          do let register = toArgument $ toEnum $ fromIntegral $ word .&. bitmask 4
                             let shift = (word `shiftR` 8) .&. bitmask 4
                             let shift_reg = toArgument $ toEnum $ fromIntegral $ shift
                             return $ Just (register, LSR shift_reg)
                       else return Nothing

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
