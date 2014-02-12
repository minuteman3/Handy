module Main where
import Handy.CPU
import Handy.Memory
import Handy.Registers
import Handy.Instructions
import Handy.StatusRegister
import Control.Monad.State

testProg :: Memory
testProg = [MOV AL R0 (ArgC 10) NoShift,
            MOV AL R1 (ArgC 20) (LSL (ArgC 1)),
            ADD AL R2 (ArgR R0) (ArgR R1) NoShift,
            MUL AL R2 (ArgR R2) (ArgR R1),
            HALT]
-- Expect final state: R0 = 10, R1 = 40, R2 = 2000, R15 = 4, all other registers = 0, CPSR = ffff

testProg1 = [MOV AL R0 (ArgC 10) NoShift,
             SUB AL R0 (ArgR R0) (ArgC 11) NoShift,
             HALT]

testProg2 = [MOV AL R0 (ArgC 2) NoShift,
             MOV AL R1 (ArgC 1) NoShift,
             ADD AL R1 (ArgR R1) (ArgC 1) NoShift,
             CMP AL (ArgR R1) (ArgC 10) NoShift,
             BL NE (ArgC $ negate 5),
             HALT]
-- Expect final state: R0 = 2, R1 = 10, R14 = 4, R15 = 5, all other registers = 0, CPSR = ftff

testProg3 = [MOV AL R0 (ArgC 1) NoShift,
             EOR AL R0 (ArgR R0) (ArgC 3) NoShift,
             HALT]

newMachine :: Memory -> Machine
newMachine mem = Machine { registers = blankRegisterFile
                         , memory    = mem
                         , cpsr      = blankStatusRegister
                         , executing = True
                         , fetchR    = Nothing
                         , decodeR   = Nothing
                         , executeR  = Nothing
                         }

runCPU :: Memory -> IO ((), Machine)
runCPU mem = runStateT run (newMachine mem)
