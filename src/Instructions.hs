{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Handy.Instructions where
import Handy.Registers (Register,RegisterFile,get)
import Data.Int (Int32)
import Data.List (elem)
import Prelude hiding (EQ,LT,GT)

type Destination = Register
type Constant = Int32

data Argument a where
    ArgC :: Constant -> Argument Constant
    ArgR :: Register -> Argument Register

instance Show (Argument a) where
    show (ArgC v) = "#" ++ (show v)
    show (ArgR r) = show r

eval :: Argument a -> RegisterFile -> Int32
eval (ArgC v) _  = v
eval (ArgR r) rf = rf `get` r

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
               deriving Eq

instance Show Condition where
    show AL = ""
    show EQ = "EQ"
    show NE = "NE"
    show CS = "CS"
    show HS = "HS"
    show CC = "CC"
    show LO = "LO"
    show MI = "MI"
    show PL = "PL"
    show VS = "VS"
    show VC = "VC"
    show HI = "HI"
    show LS = "LS"
    show GE = "GE"
    show LT = "LT"
    show GT = "GT"
    show LE = "LE"

data ShiftOp a = LSL (Argument a) -- Logical shift left
                | LSR (Argument a) -- Logical shift right
                | ASR (Argument a) -- Arithmetic shift right
                | ROR (Argument a) -- Rotate right
                | RRX -- Rotate right + sign extend
                | NoShift
                deriving (Eq)

instance Show (ShiftOp a) where
    show (LSL a) = ", LSL " ++ show a
    show (LSR a) = ", LSR " ++ show a
    show (ASR a) = ", ASR " ++ show a
    show (ROR a) = ", ROR " ++ show a
    show RRX     = ", RRX"
    show NoShift = ""

{--
    TODO: Add `S` flag to instructions

          Either need to create: duplicate instructions, eg. ADD/ADDS, SUB/SUBS
                                 add another argument to data constructors, eg.
                                     ADD :: Condition -> UpdateFlag -> Dest...
 --}

data Instruction where
    ADD  :: Condition -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    SUB  :: Condition -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    RSB  :: Condition -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    MUL  :: Condition -> Destination -> Argument Register -> Argument Register -> Instruction
    CMP  :: Condition -> Argument Register  -> Argument a -> ShiftOp b -> Instruction
    MOV  :: Condition -> Destination -> Argument a -> ShiftOp b -> Instruction
    MVN  :: Condition -> Destination -> Argument a -> ShiftOp b -> Instruction
    NEG  :: Condition -> Destination -> Argument a -> ShiftOp b -> Instruction
    -- FIXME: B and BL are supposed to take a label argument but label isn't implemented yet.
    --        Potentially okay to just use constants (this is how the ISA is implemented) but
    --        a mechanism for labels that works correctly has to be found.
    B    :: Condition -> Argument Constant  -> Instruction
    BL   :: Condition -> Argument Constant  -> Instruction
    BX   :: Condition -> Argument Register -> Instruction
    HALT :: Instruction -- FIXME: Not a real instruction. Try and come up with alternative.

instance Show Instruction where
    show (ADD cond dest src1 src2 shft) = "ADD" ++ stringify3aryOp cond dest src1 src2 shft
    show (SUB cond dest src1 src2 shft) = "SUB" ++ stringify3aryOp cond dest src1 src2 shft
    show (RSB cond dest src1 src2 shft) = "RSB" ++ stringify3aryOp cond dest src1 src2 shft
    show (MUL cond dest src1 src2) = "MUL" ++ show cond ++ " " ++ show dest ++ ", "
                                           ++ show src1 ++ ", " ++ show src2
    show (CMP cond src1 src2 shft) = "CMP" ++ show cond ++ " " ++ show src1 ++ ", " ++ show src2 ++ show shft
    show (MOV cond dest src1 shft) = "MOV" ++ show cond ++ " " ++ show dest ++ ", " ++ show src1 ++ show shft
    show (MVN cond dest src1 shft) = "MVN" ++ show cond ++ " " ++ show dest ++ ", " ++ show src1 ++ show shft
    show (NEG cond dest src1 shft) = "NEG" ++ show cond ++ " " ++ show dest ++ ", " ++ show src1 ++ show shft
    show (B cond src1)             = "B "  ++ show cond ++ " " ++ show src1
    show (BX cond src1)            = "BX " ++ show cond ++ " " ++ show src1
    show (BL cond src1)            = "BL " ++ show cond ++ " " ++ show src1
    show (HALT)               = ""

stringify3aryOp :: Condition -> Destination -> Argument Register -> Argument a -> ShiftOp b -> String
stringify3aryOp cond dest src1 src2 shft = show cond ++ " " ++ show dest ++ ", " ++ show src1
                                                     ++ ", " ++ show src2 ++ show shft
