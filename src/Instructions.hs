{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Handy.Instructions where
import Handy.Registers (Register)
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

{--
    TODO: Add `S` flag to instructions

          Either need to create: duplicate instructions, eg. ADD/ADDS, SUB/SUBS
                                 add another argument to data constructors, eg.
                                     ADD :: Condition -> UpdateFlag -> Dest...
 --}

data Instruction where
    ADD  :: Condition -> Destination -> Argument Register -> Argument a -> Instruction
    SUB  :: Condition -> Destination -> Argument Register -> Argument a -> Instruction
    RSB  :: Condition -> Destination -> Argument Register -> Argument a -> Instruction
    MUL  :: Condition -> Destination -> Argument Register -> Argument Register -> Instruction
    CMP  :: Condition -> Argument Register  -> Argument a -> Instruction
    MOV  :: Condition -> Destination -> Argument a -> Instruction
    NEG  :: Condition -> Destination -> Argument a -> Instruction
    -- FIXME: B and BL are supposed to take a label argument but label isn't implemented yet.
    --        Potentially okay to just use constants (this is how the ISA is implemented) but
    --        a mechanism for labels that works correctly has to be found.
    B    :: Condition -> Argument Constant  -> Instruction
    BL   :: Condition -> Argument Constant  -> Instruction
    BX   :: Condition -> Argument Register -> Instruction
    HALT :: Instruction -- FIXME: Not a real instruction. Try and come up with alternative.

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
    show (B cond src1)             = "B "  ++ show cond ++ " " ++ show src1
    show (BX cond src1)            = "BX " ++ show cond ++ " " ++ show src1
    show (BL cond src1)            = "BL " ++ show cond ++ " " ++ show src1
    show (HALT)               = ""
