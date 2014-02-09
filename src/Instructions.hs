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

deriving instance Eq a => Eq (Argument a)

data Condition = EQ -- Equal / equals zero  | Zero flag set
               | NE -- Not equal            | Zero flag clear
               | CS -- Carry set            | Carry flag set
               | HS -- Unsigned higher/same | Carry flag set
               | CC -- Carry clear          | Carry flag clear
               | LO -- Unsigned lower       | Carry flag clear
               | MI -- Minus/negative       | Negative flag set
               | PL -- Plus - postive/zero  | Negative flag clear
               | VS -- Overflow             | Overflow flag set
               | VC -- No overflow          | Overflow flag clear
               | HI -- Unsigned higher      | Carry flag set and Zero flag clear
               | LS -- Unsigned lower/same  | Carry flag clear or Zero flag set
               | GE -- Signed greater/equal | Negative flag === Overflow flag
               | LT -- Signed less than     | Negative flag !== Overflow flag
               | GT -- Signed greater than  | Zero flag clear and Negative flag === Overflow flag
               | LE -- Signed less/equal    | Zero flag set or Negative flag !== Overflow flag
               | AL -- Always               | True
               | NV -- Never                | False
               deriving (Show, Eq)

data Instruction where
    ADD  :: Condition -> Destination -> Argument a -> Argument b -> Instruction
    SUB  :: Condition -> Destination -> Argument a -> Argument b -> Instruction
    RSB  :: Condition -> Destination -> Argument a -> Argument b -> Instruction
    MUL  :: Condition -> Destination -> Argument a -> Argument Register -> Instruction
    CMP  :: Condition -> Argument a  -> Argument b -> Instruction
    MOV  :: Condition -> Destination -> Argument a -> Instruction
    NEG  :: Condition -> Destination -> Argument a -> Instruction
    B    :: Condition -> Argument a  -> Instruction
    BL   :: Condition -> Argument a  -> Instruction
    BX   :: Condition -> Argument Register -> Instruction
    HALT :: Instruction

instance Show Instruction where
    show (ADD cond dest src1 src2) = "ADD" ++ show cond ++ " " ++ show dest ++ ", "
                                           ++ show src1 ++ ", " ++ show src2
    show (SUB cond dest src1 src2) = "SUB" ++ show cond ++ " " ++ show dest ++ ", "
                                           ++ show src1 ++ ", " ++ show src2
    show (RSB cond dest src1 src2) = "RSB" ++ show cond ++ " " ++ show dest ++ ", "
                                           ++ show src1 ++ ", " ++ show src2
    show (MUL cond dest src1 src2) = "MUL" ++ show cond ++ " " ++ show dest ++ ", "
                                           ++ show src1 ++ ", " ++ show src2
    show (CMP cond src1 src2)      = "CMP" ++ show cond ++ " " ++ show src1 ++ ", " ++ show src2
    show (MOV cond dest src1)      = "MOV" ++ show cond ++ " " ++ show dest ++ ", " ++ show src1
    show (NEG cond dest src1)      = "NEG" ++ show cond ++ " " ++ show dest ++ ", " ++ show src1
    show (B cond src1)             = "B "  ++ show cond ++ show src1
    show (BX cond src1)            = "BX " ++ show cond ++ show src1
    show (BL cond src1)            = "BL " ++ show cond ++ show src1
    show (HALT)               = ""
