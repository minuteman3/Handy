{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Handy.Instructions where
import Handy.Registers (Register)
import Data.Int (Int32)
import Data.List (elem)

type Destination = Register
type Constant = Int32

data Argument a where
    ArgC :: Constant -> Argument Constant
    ArgR :: Register -> Argument Register

instance Show (Argument a) where
    show (ArgC v) = "#" ++ (show v)
    show (ArgR r) = show r

data Instruction where
    ADD  :: Destination -> Argument a -> Argument a -> Instruction
    SUB  :: Destination -> Argument a -> Argument a -> Instruction
    RSB  :: Destination -> Argument a -> Argument a -> Instruction
    MUL  :: Destination -> Argument a -> Argument Register -> Instruction
    CMP  :: Argument a  -> Argument a -> Instruction
    MOV  :: Destination -> Argument a -> Instruction
    NEG  :: Destination -> Argument a -> Instruction
    B    :: Argument a  -> Instruction
    BX   :: Argument a  -> Instruction
    BL   :: Argument a  -> Instruction
    HALT :: Instruction

instance Show Instruction where
    show (ADD dest src1 src2) = "ADD " ++ show dest ++ ", " ++ show src1 ++ ", " ++ show src2
    show (SUB dest src1 src2) = "SUB " ++ show dest ++ ", " ++ show src1 ++ ", " ++ show src2
    show (RSB dest src1 src2) = "RSB " ++ show dest ++ ", " ++ show src1 ++ ", " ++ show src2
    show (MUL dest src1 src2) = "MUL " ++ show dest ++ ", " ++ show src1 ++ ", " ++ show src2
    show (CMP src1 src2)      = "CMP " ++ show src1 ++ ", " ++ show src2
    show (MOV dest src1)      = "MOV " ++ show dest ++ ", " ++ show src1
    show (NEG dest src1)      = "NEG " ++ show dest ++ ", " ++ show src1
    show (B src1)             = "B "   ++ show src1
    show (BX src1)            = "BX "  ++ show src1
    show (BL src1)            = "BL "  ++ show src1
    show (HALT)               = ""
