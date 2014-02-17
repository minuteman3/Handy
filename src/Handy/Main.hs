module Main where
import Handy.CPU
import Handy.Memory
import Handy.Registers
import Handy.Instructions
import Handy.StatusRegister
import Handy.Encoder
import Control.Monad.State

type Program = [Instruction]

toMemory :: [Instruction] -> Memory
toMemory is = foldr (\(a,i) mem -> writeWord mem a i) blankMemory $ zip [0,4..] $ map serialiseInstruction is

{-testProg :: Program-}
{-testProg = [ADD AL NoS R0 (ArgR R0) (ArgC 1024) NoShift-}
           {-,ADD AL NoS R0 (ArgR R0) (ArgC 1) NoShift-}
           {-,ADD AL NoS R0 (ArgR R0) (ArgC 1) NoShift-}
           {-,ADD AL NoS R0 (ArgR R0) (ArgC 1) NoShift-}
           {-,ADD AL NoS R0 (ArgR R0) (ArgC 1) NoShift-}
           {-,ADD AL NoS R0 (ArgR R0) (ArgC 1) NoShift-}
           {-,MOV AL NoS R1 (ArgC 1024) NoShift-}
           {-,MVN AL NoS R2 (ArgC 1) NoShift-}
           {-,HALT]-}

{-testProg1 :: Program-}
{-testProg1 = [CMP AL (ArgR R0) (ArgC 1) NoShift,HALT]-}

testProg :: Program
testProg =  [MOV AL NoS R0 (ArgC 10) NoShift,
            MOV AL NoS R1 (ArgC 20) NoShift,
            MOV AL NoS R1 (ArgR R1) (LSL (ArgC 1)),
            ADD AL NoS R2 (ArgR R0) (ArgR R1) NoShift,
            MUL AL NoS R2 (ArgR R2) (ArgR R1),
            HALT]
-- Expect final state: R0 = 10, R1 = 40, R2 = 2000, R15 = 4, all other registers = 0, CPSR = ffff

{-testProg1 = [MOV AL NoS R0 (ArgC 10) NoShift,-}
             {-SUB AL NoS R0 (ArgR R0) (ArgC 11) NoShift,-}
             {-HALT]-}

testProg2 :: Program
testProg2 = [MOV AL NoS R0 (ArgC 8) NoShift,
             MOV AL NoS R1 (ArgC 1) NoShift,
             ADD AL NoS R1 (ArgR R1) (ArgC 1) NoShift,
             CMP AL (ArgR R1) (ArgC 10) NoShift,
             BL NE (ArgC $ negate 5),
             HALT]
-- Expect final state: R0 = 2, R1 = 10, R14 = 4, R15 = 5, all other registers = 0, CPSR = ftff

testProg3 :: Program
testProg3 = [MOV AL NoS R0 (ArgC 1) NoShift,
             EOR AL NoS R0 (ArgR R0) (ArgC 3) NoShift,
             HALT]

testProg4 :: Program
testProg4 = [MOV AL NoS R0 (ArgC 2) NoShift, SUB AL S R0 (ArgR R0) (ArgC 1) NoShift, HALT]

testProg5 :: Program
testProg5 = [MOV AL NoS R0 (ArgC 128) NoShift
            ,STRB AL (ArgR R0) (ImmPostIndex (ArgR R0) (ArgC 1) Up)
            ,STRB AL (ArgR R0) (ImmPostIndex (ArgR R0) (ArgC 1) Up)
            ,STRB AL (ArgR R0) (ImmPostIndex (ArgR R0) (ArgC 1) Up)
            ,STRB AL (ArgR R0) (ImmPostIndex (ArgR R0) (ArgC 1) Up)
            ,HALT]

newMachine :: Memory -> Machine
newMachine mem = Machine { registers = blankRegisterFile
                         , memory    = mem
                         , cpsr      = blankStatusRegister
                         , executing = True
                         , fetchR    = Nothing
                         , decodeR   = Nothing
                         , executeR  = Nothing
                         }

runCPU :: Program -> IO Machine
runCPU prog = execStateT run (newMachine $ toMemory prog)
