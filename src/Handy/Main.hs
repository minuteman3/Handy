module Main where
import Handy.CPU
import Handy.Memory
import Handy.Registers
import Handy.Instructions
import Handy.StatusRegister
import Handy.Encoder
import qualified Data.ByteString as B
import qualified Data.List as DL
import System.Environment
import Data.Word
import Data.Bits
import Control.Monad.State
import Prelude hiding(GT,LE,EQ)

type Program = [Instruction]

toMemoryBinary :: [Word32] -> Memory
toMemoryBinary x = foldr (\(a,i) mem -> writeWord mem a i) blankMemory $ zip [0,4..] x

toMemory :: [Instruction] -> Memory
toMemory is = toMemoryBinary $ map serialiseInstruction is

demoProg :: Program
demoProg = [MOV AL NoS R0 (ArgC 32) NoShift,
            MOV AL NoS R1 (ArgC 14) NoShift,
            CMP AL (ArgR R0) (ArgR R1) NoShift,
            SUB GT NoS R0 (ArgR R0) (ArgR R1) NoShift,
            SUB LE NoS R1 (ArgR R1) (ArgR R0) NoShift,
            B   NE (ArgC $ negate 6),
            HALT]

newMachine :: Memory -> Machine
newMachine mem = Machine { registers   = blankRegisterFile
                         , memory      = mem
                         , cpsr        = blankStatusRegister
                         , executing   = True
                         , fetchR      = Nothing
                         , decodeR     = Nothing
                         , executeR    = Nothing
                         , stall       = 0
                         , totalCycles = 0
                         }

runCPU :: Program -> IO Machine
runCPU prog = execStateT run (newMachine $ toMemory prog)

group :: Int -> [a] -> [[a]]
group x [] = []
group x l = first : group x rest
      where (first, rest) = splitAt x l

toWord32List :: B.ByteString -> [Word32]
toWord32List = map octetToWord . group 4 . B.unpack

octetToWord :: [Word8] -> Word32
octetToWord = DL.foldl' addup 0
  where addup a o = (a `shiftL` 8) .|. fromIntegral o

main :: IO ()
main = do
    args <- getArgs
    let inputfile = head args
    binarydata <- B.readFile inputfile
    let program = toWord32List binarydata
    final <- execStateT run (newMachine $ toMemoryBinary program)
    putStrLn $ show final

testProg :: Program
testProg =  [MOV AL NoS R0 (ArgC 10) NoShift,
            MOV AL NoS R1 (ArgC 20) NoShift,
            MOV AL NoS R1 (ArgR R1) (LSL (ArgC 1)),
            ADD AL NoS R2 (ArgR R0) (ArgR R1) NoShift,
            MUL AL NoS R2 (ArgR R2) (ArgR R1),
            HALT]
-- Expect final state: R0 = 10, R1 = 40, R2 = 2000, R15 = 4, all other registers = 0, CPSR = ffff

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

testProg6 :: Program
testProg6 = [MOV AL NoS R0 (ArgC 128) NoShift
            ,MOV AL NoS R1 (ArgC 1)   NoShift
            ,MOV AL NoS R2 (ArgC 2)   NoShift
            ,MOV AL NoS R3 (ArgC 3)   NoShift
            ,STM AL EA (ArgR R0) NoUpdate [R1,R2,R3]
            ,LDM AL EA (ArgR R0) Update   [R4,R5,R6]
            ,HALT
            ]

testProg7 :: Program
testProg7 = [MOV AL NoS R0 (ArgC 1) NoShift
            ,MOV AL NoS R0 (ArgR R0) (ROR (ArgC 1))
            ,MOV AL NoS R1 (ArgC 2) NoShift
            ,SMULL AL NoS R2 R3 (ArgR R0) (ArgR R1)
            ,HALT]
