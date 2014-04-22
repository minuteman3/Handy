{-# LANGUAGE GADTs #-}
module Handy.CPU where

import Prelude hiding (EQ,LT,GT)

import Handy.Memory
import Handy.Instructions
import Handy.StatusRegister
import Handy.ALU
import Handy.Decoder
import qualified Handy.Registers as Reg

import Control.Monad.State
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Bits ((.&.))
import qualified Data.ByteString.Lazy as B

type InstructionWord = B.ByteString
type FetchRegister   = Maybe InstructionWord
type DecodeRegister  = Maybe Instruction
type ExecuteRegister = Maybe Instruction

data Machine = Machine { registers   :: Reg.RegisterFile
                       , memory      :: Memory
                       , cpsr        :: StatusRegister
                       , fetchR      :: FetchRegister
                       , decodeR     :: DecodeRegister
                       , executeR    :: ExecuteRegister
                       , stall       :: Word8
                       , executing   :: Bool
                       , totalCycles :: Integer
                       } deriving Show

type CPU a = StateT Machine IO a
run :: CPU ()
run = do
    running <- gets executing
    when running $ do
        stalled <- isStalled
        modify incCycles
        if stalled then
            modify reduceStall
        else do
            modify pipeline
            execute =<< gets executeR
            modify incPC
        run

pipeline :: Machine -> Machine
pipeline machine = machine { fetchR   = nextInstruction machine
                           , decodeR  = decode $ fetchR machine
                           , executeR = decodeR machine
                           }

flushPipeline :: Machine -> Machine
flushPipeline machine = machine { fetchR   = Nothing
                                , decodeR  = Nothing
                                , executeR = Nothing
                                }

nextInstruction :: Machine -> Maybe InstructionWord
nextInstruction machine = let pc = fromIntegral $ registers machine `Reg.get` Reg.PC in
                              Just $ B.pack $ memory machine `getInstructionWord` pc

setRegister :: Reg.Register -> Int32 -> Machine -> Machine
setRegister r v machine = machine { registers = Reg.set (registers machine) r v }

incPC :: Machine -> Machine
incPC machine = let pc = registers machine `Reg.get` Reg.PC in
                  setRegister Reg.PC ((pc .&. 0xFFFFFFFC) + 4) machine

incCycles :: Machine -> Machine
incCycles machine = machine { totalCycles = totalCycles machine + 1 }

isStalled :: CPU Bool
isStalled = do s <- gets stall
               return $ s > 0

reduceStall :: Machine -> Machine
reduceStall machine = machine { stall = stall machine - 1 }

stallMachine :: Instruction -> Machine -> Machine
stallMachine (LDR{})  machine = machine { stall = 10 }
stallMachine (LDRB{}) machine = machine { stall = 10 }
stallMachine (STR{})  machine = machine { stall = 10 }
stallMachine (STRB{}) machine = machine { stall = 10 }
stallMachine (LDM{})  machine = machine { stall = 10 }
stallMachine (STM{})  machine = machine { stall = 10 }
stallMachine _        machine = machine

execute :: Maybe Instruction -> CPU ()
execute Nothing = return ()
execute (Just i) =
    do rf_pre <- gets registers
       execute' i
       modify $ stallMachine i
       rf_post <- gets registers
       when (rf_pre `Reg.get` Reg.PC /= rf_post `Reg.get` Reg.PC) $
           modify flushPipeline

execute' :: Instruction -> CPU ()

execute' JunkInstruction = error "Attempted to execute an unimplemented instruction"

execute' HALT = state (\machine -> ((),machine { executing = False }))

execute' (B cond src) =
    do machine <- get
       when (checkCondition cond $ cpsr machine) $ do
           let (rf,_) = computeBranch src cond (registers machine) (cpsr machine)
           put $ machine { registers = rf }

execute' (BL cond src) =
    do machine <- get
       let rf = registers machine
       when (checkCondition cond $ cpsr machine) $ do
           let link = (rf `Reg.get` Reg.PC) - 4
           modify $ setRegister Reg.LR link
       execute' (B cond src)

execute' (BX cond src) =
    do machine <- get
       let rf = registers machine
           sr = cpsr machine
       when (checkCondition cond sr) $ do
           let (rf',_) = compute (AND cond NoS Reg.PC src (ArgC 0xFFFFFFFE) NoShift) rf sr
           let (rf'',_) = compute (SUB cond NoS Reg.PC src (ArgC 4) NoShift) rf' sr
           put $ machine { registers = rf'' }

execute' (LDR cond (ArgR dest) addrm) =
    do machine@(Machine rf mem sr _ _ _ _ _ _) <- get
       when (checkCondition cond sr) $ do
           let (addr,rf') = computeAddress addrm rf sr
               rf'' = Reg.set rf' dest (fromIntegral (mem `getWord` addr))
           put $ machine { registers = rf'' }


execute' (LDRB cond (ArgR dest) addrm) =
    do machine@(Machine rf mem sr _ _ _ _ _ _) <- get
       when (checkCondition cond sr) $ do
           let (addr,rf') = computeAddress addrm rf sr
               rf'' = Reg.set rf' dest (fromIntegral (mem `getByte` addr))
           put $ machine { registers = rf'' }

execute' (STR cond (ArgR src) addrm) =
    do machine@(Machine rf mem sr _ _ _ _ _ _) <- get
       when (checkCondition cond sr) $ do
           let (addr,rf') = computeAddress addrm rf sr
               mem' = writeWord mem addr (fromIntegral (rf  `Reg.get` src))
           put $ machine { registers = rf', memory = mem' }

execute' (STRB cond (ArgR src) addrm) =
    do machine@(Machine rf mem sr _ _ _ _ _ _) <- get
       when (checkCondition cond sr) $ do
           let (addr,rf') = computeAddress addrm rf sr
               mem' = writeByte mem addr (fromIntegral (rf  `Reg.get` src))
           put $ machine { registers = rf', memory = mem' }

execute' (STM cond addrm (ArgR src) update regs) =
    do machine@(Machine rf mem sr _ _ _ _ _ _) <- get
       when (checkCondition cond sr) $ do
        let addr = rf `Reg.get` src
            (start,end) = computeStartEnd addr addrm regs
            range = [start,start+4,end]
            vals = map (Reg.get rf) regs
            assoc = zip range vals
            mem' = foldl writeMem mem assoc
            rf' = if update == Update then
                     updateReg start end addrm rf src
                  else rf
        put $ machine { registers = rf', memory = mem' }

execute' (LDM cond addrm (ArgR src) update regs) =
    do machine@(Machine rf mem sr _ _ _ _ _ _) <- get
       when (checkCondition cond sr) $ do
           let addr = rf `Reg.get` src
               (start,end) =  computeStartEnd addr addrm regs
               range = map fromIntegral [start,start+4,end]
               vals = map (getWord mem) range
               assoc = zip regs vals
               rf' = foldl writeReg rf assoc
               rf'' = if update == Update then
                         updateReg start end addrm rf' src
                      else rf'
           put $ machine { registers = rf'' }


execute' i = do machine <- get
                when (checkCondition (getCondition i) (cpsr machine)) $ do
                    let (rf,sr) = compute i (registers machine) (cpsr machine)
                    put $ machine { cpsr = sr, registers = rf }

writeReg :: Integral a => Reg.RegisterFile -> (Reg.Register, a) -> Reg.RegisterFile
writeReg rf (reg,v) = Reg.set rf reg (fromIntegral v)

writeMem :: Memory -> (Int32, Int32) -> Memory
writeMem mem (addr,v) = writeWord mem (fromIntegral addr) (fromIntegral v)

computeStartEnd :: Int32 -> AddressingModeMulti -> [Reg.Register] -> (Int32,Int32)
computeStartEnd addr addrm regs =
    (fromIntegral start, fromIntegral end)
         where (start, end)  = case addrm of
                         IA -> (addr, addr + fromIntegral (length regs) * 4 - 4)
                         IB -> (addr + 4, addr + fromIntegral (length regs) * 4)
                         DA -> (addr - fromIntegral (length regs) * 4 + 4, addr)
                         DB -> (addr - fromIntegral (length regs) * 4, addr - 4)

updateReg :: Int32 -> Int32 -> AddressingModeMulti -> Reg.RegisterFile -> Reg.Register -> Reg.RegisterFile
updateReg start end addrm rf src = rf'
    where rf' = case addrm of
           IA -> Reg.set rf src (end + 4)
           IB -> Reg.set rf src end
           DA -> Reg.set rf src (start - 4)
           DB -> Reg.set rf src start
