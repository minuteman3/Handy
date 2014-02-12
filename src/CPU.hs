{-# LANGUAGE GADTs #-}
module Handy.CPU where

import Prelude hiding (EQ,LT,GT)

import Handy.Memory
import Handy.Instructions
import Handy.StatusRegister
import Handy.ALU
import Handy.Util (computeBranchOffset, checkCondition)
import qualified Handy.Registers as Reg

import Control.Monad.State
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Bits

type FetchRegister   = Maybe Instruction
type DecodeRegister  = Maybe Instruction
type ExecuteRegister = Maybe Instruction

data Machine = Machine { registers :: Reg.RegisterFile
                       , memory    :: Memory
                       , cpsr      :: StatusRegister
                       , executing :: Bool
                       , fetchR    :: FetchRegister
                       , decodeR   :: DecodeRegister
                       , executeR  :: ExecuteRegister
                       } deriving Show

type Run a = StateT Machine IO a
run :: Run ()
run = do
    running <- gets executing
    when (running) $ do
        modify pipeline
        i <- gets executeR
        execute i
        modify incPC
        run

{-
    TODO/FIXME:
    Currently decode is a no op because instructions arrive fully formed.
    Later, fetchR should be a "Maybe Word32", and there should be an instruction
    decoder with type Word32 -> Instruction that gets applied to the contents of
    decodeR as they move into executeR
-}
pipeline :: Machine -> Machine
pipeline machine = machine { fetchR   = nextInstruction machine
                           , decodeR  = fetchR machine
                           , executeR = decodeR machine
                           }

flushPipeline :: Machine -> Machine
flushPipeline machine = machine { fetchR   = Nothing
                                , decodeR  = Nothing
                                , executeR = Nothing
                                }

{-
    TODO/FIXME:
    All this bounds checking is only necessary because of the current temporary
    implementation of memory as a list of instructions. Later on this function
    should be simplified a lot by just being a call to the memory.
-}
nextInstruction :: Machine -> Maybe Instruction
nextInstruction machine = let pc = fromIntegral $ (registers machine) `Reg.get` Reg.PC in
                             if pc <= (4 * length (memory machine)) - 1  then
                                Just $ (memory machine) !! (pc `div` 4)
                             else Nothing

incPC :: Machine -> Machine
incPC machine = let pc = (registers machine) `Reg.get` Reg.PC in
                  setRegister Reg.PC (pc + 4) machine

execute :: Maybe Instruction -> Run ()
execute Nothing = return ()
execute (Just i) = execute' i

execute' :: Instruction -> Run ()
execute' HALT = state $ (\machine -> ((),machine { executing = False }))
execute' (MOV cond dest src shft)       = executeUnOp id cond dest src shft
execute' (MVN cond dest src shft)       = executeUnOp complement cond dest src shft
execute' (ADD cond dest src1 src2 shft) = executeBinOp (+) cond dest src1 src2 shft
execute' (SUB cond dest src1 src2 shft) = executeBinOp (-) cond dest src1 src2 shft
execute' (RSB cond dest src1 src2 shft) = executeBinOp (-) cond dest src2 src1 shft
execute' (AND cond dest src1 src2 shft) = executeBinOp (.&.) cond dest src2 src1 shft
execute' (ORR cond dest src1 src2 shft) = executeBinOp (.|.) cond dest src2 src1 shft
execute' (EOR cond dest src1 src2 shft) = executeBinOp xor cond dest src2 src1 shft
execute' (MUL cond dest src1 src2)      = executeBinOp (*) cond dest src1 src2 NoShift

execute' (CMP cond src1 src2 shft) = do machine <- get
                                        let sr = (cpsr machine)
                                        when (checkCondition cond $ cpsr machine) $ do
                                             let result = computeBinOp (-) src1 src2 (registers machine) shft
                                             let cpsr' = setCPSR result sr
                                             put $ machine { cpsr = cpsr' }

execute' (B cond src) = do machine <- get
                           let rf = registers machine
                           when (checkCondition cond $ cpsr machine) $ do
                               let offset = computeBranchOffset src
                               executeBinOp (+) cond Reg.PC (ArgR Reg.PC) offset NoShift
                               modify $ flushPipeline


execute' (BL cond src) = do machine <- get
                            let rf = registers machine
                            when (checkCondition cond $ cpsr machine) $ do
                                let link = (rf `Reg.get` Reg.PC) - 4
                                modify $ setRegister Reg.LR link
                            execute' (B cond src)

execute' (BX cond src) = executeUnOp (.&. 0xFFFFFFFE) cond Reg.PC src NoShift

executeUnOp :: (Int32 -> Int32) -> Condition -> Destination -> Argument a -> ShiftOp b -> Run ()
executeUnOp op cond dest src shft = do machine <- get
                                       when (checkCondition cond $ cpsr machine) $ do
                                            let result = computeUnOp op src (registers machine) shft
                                            modify $ setRegister dest result

executeBinOp :: (Int32 -> Int32 -> Int32)
             -> Condition
             -> Destination
             -> Argument a
             -> Argument b
             -> ShiftOp c
             -> Run ()

executeBinOp op cond dest src1 src2 shft = do machine <- get
                                              let rf = registers machine
                                              when (checkCondition cond $ cpsr machine) $ do
                                                   let result = computeBinOp op src1 src2 rf shft
                                                   modify $ setRegister dest result



computeUnOp :: (Int32 -> Int32) -> Argument a -> Reg.RegisterFile -> ShiftOp b -> Int32
computeUnOp op src rf shft = op v'
                             where v  = src `eval` rf
                                   v' = fromIntegral $ computeShift v shft rf

computeBinOp :: (Int32 -> Int32 -> Int32) -> Argument a -> Argument b -> Reg.RegisterFile -> ShiftOp c -> Int32
computeBinOp op src1 src2 rf shft = result
                                    where va  = src1 `eval` rf
                                          vb  = src2 `eval` rf
                                          vb' = computeShift vb shft rf
                                          result = va `op` vb'

setCPSR :: Int32 -> StatusRegister -> StatusRegister
setCPSR result cpsr = cpsr { negative = testBit result 31
                           , zero     = result == 0
                           -- TODO: Overflow and Carry flag implementation
                           }



setRegister :: Reg.Register -> Int32 -> Machine -> Machine
setRegister r v machine = machine { registers = Reg.set (registers machine) r v }
