{-# LANGUAGE GADTs #-}
module Handy.CPU where

import Prelude hiding (EQ,LT,GT)

import Handy.Memory
import Handy.Instructions
import Handy.StatusRegister
import qualified Handy.Registers as Reg

import Control.Monad.State
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int32)
import Data.Bits

data Machine = Machine { registers :: Reg.RegisterFile
                       , memory :: Memory
                       , cpsr   :: StatusRegister
                       } deriving Show

type Run a = StateT Machine IO a
run :: Run ()
run = do
    i <- nextInstruction
    execute i

nextInstruction :: Run Instruction
nextInstruction = do machine <- get
                     let pc = (registers machine) `Reg.get` Reg.PC
                     return $ (memory machine) !! (fromIntegral pc)

incPC :: Run ()
incPC = do machine <- get
           let pc = (registers machine) `Reg.get` Reg.PC
           setRegister Reg.PC (pc + 1)

execute :: Instruction -> Run ()
execute HALT = return ()
execute (MOV cond dest src)       = executeUnOp id cond dest src
execute (NEG cond dest src)       = executeUnOp negate cond dest src
execute (ADD cond dest src1 src2) = executeBinOp (+) cond dest src1 src2
execute (SUB cond dest src1 src2) = executeBinOp (-) cond dest src1 src2
execute (RSB cond dest src1 src2) = executeBinOp (-) cond dest src2 src1
execute (MUL cond dest src1 src2) = executeBinOp (*) cond dest src1 src2

execute (CMP cond src1 src2)      = do machine <- get
                                       when (checkCondition cond $ cpsr machine) $ do
                                           let result = computeBinOp (-) src1 src2 (registers machine)
                                           let cpsr' = setCPSR result (cpsr machine)
                                           put $ machine { cpsr = cpsr' }
                                       incPC
                                       run

-- FIXME: Does not account for the fact B argument can only be +/- 16MB on real processor
--        Correct semantics: PC := PC + (SignExtend_30(signed_immed_24) << 2)
--        Current semantics: PC := src
execute (B cond src)              = executeUnOp id cond Reg.PC src

execute (BL cond src)             = do machine <- get
                                       let rf = registers machine
                                       when (checkCondition cond $ cpsr machine) $
                                            setRegister Reg.LR (rf `Reg.get` Reg.PC)
                                       execute (B cond src)

execute (BX cond src)             = executeUnOp (.&. 0xFFFFFFFE) cond Reg.PC src

executeUnOp :: (Int32 -> Int32) -> Condition -> Destination -> Argument a -> Run ()
executeUnOp op cond dest src = do machine <- get
                                  incPC
                                  when (checkCondition cond $ cpsr machine) $ do
                                       let result = computeUnOp op src (registers machine)
                                       setRegister dest result
                                  run

executeBinOp :: (Int32 -> Int32 -> Int32) -> Condition -> Destination -> Argument a -> Argument b -> Run ()
executeBinOp op cond dest src1 src2 = do machine <- get
                                         incPC
                                         when (checkCondition cond $ cpsr machine) $ do
                                              let result = computeBinOp op src1 src2 (registers machine)
                                              setRegister dest result
                                         run


computeUnOp :: (Int32 -> Int32) -> Argument a -> Reg.RegisterFile -> Int32
computeUnOp op src rf = op v
                        where v = case src of
                                   (ArgC a) -> a
                                   (ArgR a) -> rf `Reg.get` a

computeBinOp :: (Int32 -> Int32 -> Int32) -> Argument a -> Argument b -> Reg.RegisterFile -> Int32
computeBinOp op src1 src2 rf = va `op` vb
                               where va = case src1 of
                                           (ArgC a) -> a
                                           (ArgR a) -> rf `Reg.get` a
                                     vb = case (src2) of
                                           (ArgC b) -> b
                                           (ArgR b) -> rf `Reg.get` b

checkCondition :: Condition -> StatusRegister -> Bool
checkCondition AL _ = True
checkCondition EQ cpsr = zero cpsr
checkCondition CS cpsr = carry cpsr
checkCondition MI cpsr = negative cpsr
checkCondition VS cpsr = overflow cpsr
checkCondition HI cpsr = (carry cpsr) && (not $ zero cpsr)
checkCondition GE cpsr = (negative cpsr) == (overflow cpsr)

checkCondition NE cpsr = not $ checkCondition EQ cpsr
checkCondition CC cpsr = not $ checkCondition CS cpsr
checkCondition PL cpsr = not $ checkCondition MI cpsr
checkCondition VC cpsr = not $ checkCondition VS cpsr
checkCondition LS cpsr = not $ checkCondition HI cpsr
checkCondition LT cpsr = not $ checkCondition GE cpsr
checkCondition GT cpsr = (not $ zero cpsr) && (checkCondition GE cpsr)
checkCondition LE cpsr = not $ checkCondition GT cpsr

checkCondition HS cpsr = checkCondition CS cpsr
checkCondition LO cpsr = checkCondition CC cpsr

setCPSR :: Int32 -> StatusRegister -> StatusRegister
setCPSR result cpsr = cpsr { negative = testBitDefault result 31
                           , zero     = result == 0
                           -- TODO: Overflow and Carry flag implementation
                           }

setRegister :: Reg.Register -> Int32 -> Run ()
setRegister r v = state $ (\machine -> ((), machine { registers = Reg.set (registers machine) r v }))

newMachine :: Memory -> Machine
newMachine mem = Machine {registers=Reg.blankRegisterFile,memory=mem,cpsr=blankStatusRegister}

testProg :: Memory
testProg = [MOV AL Reg.R0 (ArgC 10),
            MOV AL Reg.R1 (ArgC 20),
            ADD AL Reg.R2 (ArgR Reg.R0) (ArgR Reg.R1),
            MUL AL Reg.R2 (ArgR Reg.R2) (ArgR Reg.R1),
            HALT]

testProg2 = [MOV AL Reg.R0 (ArgC 2),
             MOV AL Reg.R1 (ArgC 1),
             ADD AL Reg.R1 (ArgR Reg.R1) (ArgC 1),
             CMP AL (ArgR Reg.R1) (ArgC 10),
             {-BX  NE (ArgR Reg.R0),-}
             BL NE (ArgC 2),
             HALT]

runRun :: Memory -> IO ((), Machine)
runRun mem = runStateT run (newMachine mem)
