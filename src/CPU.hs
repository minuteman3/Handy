{-# LANGUAGE GADTs #-}
module Handy.CPU where
import Handy.Instructions
import qualified Handy.Registers as Reg
import Handy.Memory
import Control.Monad.State
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int32)

data Machine = Machine { registers :: Reg.RegisterFile, memory :: Memory } deriving Show

type Run a = StateT Machine IO a
run :: Run ()
run = do
    machine <- get
    i <- nextInstruction
    execute i

nextInstruction :: Run Instruction
nextInstruction = do machine <- get
                     return $ (memory machine) !! (fromIntegral $ ((registers machine) `Reg.get` Reg.PC))

incPC :: Run ()
incPC = do machine <- get
           setRegister Reg.PC $ ((registers machine) `Reg.get` Reg.PC) + 1

execute :: Instruction -> Run ()
execute (MOV dest src)       = executeUnOp  id  dest src
execute (ADD dest src1 src2) = executeBinOp (+) dest src1 src2
execute (SUB dest src1 src2) = executeBinOp (-) dest src1 src2
execute (RSB dest src1 src2) = executeBinOp (-) dest src2 src1
execute (MUL dest src1 src2) = executeBinOp (*) dest src1 src2
execute HALT = return ()

executeUnOp  :: (Int32 -> Int32) -> Destination -> Argument a -> Run ()
executeUnOp  op dest src = do machine <- get
                              v <- case src of
                                        (ArgC a) -> return a
                                        (ArgR a) -> getRegister a
                              setRegister dest (op v)
                              incPC
                              run

executeBinOp :: (Int32 -> Int32 -> Int32) -> Destination -> Argument a -> Argument b -> Run ()
executeBinOp op dest src1 src2 = do machine <- get
                                    va <- case (src1) of
                                                (ArgC a) -> return a
                                                (ArgR a) -> getRegister a
                                    vb <- case (src2) of
                                                (ArgC b) -> return b
                                                (ArgR b) -> getRegister b
                                    setRegister dest (va `op` vb)
                                    incPC
                                    run

getRegister :: Reg.Register -> Run Int32
getRegister r = do machine <- get
                   return $ registers machine `Reg.get` r

setRegister :: Reg.Register -> Int32 -> Run ()
setRegister r v = state $ (\machine -> ((), machine { registers = Reg.set (registers machine) r v }))

newMachine :: Memory -> Machine
newMachine mem = Machine {registers=Reg.blankRegisterFile,memory=mem}

testProg :: Memory
testProg = [MOV Reg.R0 (ArgC 10),
            MOV Reg.R1 (ArgC 20),
            ADD Reg.R2 (ArgR Reg.R0) (ArgR Reg.R1),
            MUL Reg.R2 (ArgR Reg.R2) (ArgR Reg.R1),
            HALT]
