module Handy.Instructions where
import Handy.Registers (Register)
import Data.Int (Int32)
import Data.List (elem)

type Destination = Register
type Constant = Int32

data Argument = ArgC Constant
              | ArgR Register
              deriving (Eq, Show)

data Instruction = ADD Destination Argument Argument
                 | SUB Destination Argument Argument
                 | RSB Destination Argument Argument
                 | MUL Destination Argument Constant
                 | CMP Argument Argument
                 | MOV Destination Argument
                 | NEG Destination Argument
                 | B Argument
                 | BX Argument
                 | BL Argument
                 deriving (Eq, Show)
