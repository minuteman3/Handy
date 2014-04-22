{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Handy.Instructions where
import Handy.Registers (Register,RegisterFile,get)
import Data.Int (Int32)
import Prelude hiding (EQ,LT,GT)

type Destination = Register
type Constant = Int32

class ArgVal a where

instance ArgVal Int32
instance ArgVal Register

class Arg a where
    toArgument :: (ArgVal a, Arg a) => a -> Argument a

instance Arg Int32 where
    toArgument = ArgC

instance Arg Register where
    toArgument = ArgR

data Argument a where
    ArgC :: Constant -> Argument Constant
    ArgR :: Register -> Argument Register

instance Show (Argument a) where
    show (ArgC v) = "#" ++ show v
    show (ArgR r) = show r

eval :: Argument a -> RegisterFile -> Int32
eval (ArgC v) _  = v
eval (ArgR r) rf = rf `get` r

deriving instance Eq a => Eq (Argument a)

data Condition = EQ -- Equal / equals zero  | Zero flag set
               | NE -- Not equal            | Zero flag clear
               | CS -- Carry set            | Carry flag set
               | CC -- Carry clear          | Carry flag clear
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
               | HS -- Unsigned higher/same | Carry flag set
               | LO -- Unsigned lower       | Carry flag clear
               deriving (Eq, Enum)

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
    show NV = "NV"

getCondition :: Instruction -> Condition
getCondition (ADD c _ _ _ _ _) = c
getCondition (ADC c _ _ _ _ _) = c
getCondition (BIC c _ _ _ _ _) = c
getCondition (SUB c _ _ _ _ _) = c
getCondition (SBC c _ _ _ _ _) = c
getCondition (RSB c _ _ _ _ _) = c
getCondition (RSC c _ _ _ _ _) = c
getCondition (AND c _ _ _ _ _) = c
getCondition (ORR c _ _ _ _ _) = c
getCondition (EOR c _ _ _ _ _) = c
getCondition (MUL c _ _ _ _)   = c
getCondition (SMULL c _ _ _ _ _)   = c
getCondition (SMLAL c _ _ _ _ _)   = c
getCondition (MLA c _ _ _ _ _)   = c
getCondition (CMP c _ _ _)   = c
getCondition (CMN c _ _ _)   = c
getCondition (TEQ c _ _ _)   = c
getCondition (TST c _ _ _)   = c
getCondition (MOV c _ _ _ _)   = c
getCondition (MVN c _ _ _ _)   = c
getCondition (B  c _)        = c
getCondition (BL c _)        = c
getCondition (BX c _)        = c
getCondition HALT = undefined
getCondition JunkInstruction = undefined

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

data S = NoS
       | S
       deriving (Eq, Enum)

instance Show S where
    show S   = "S"
    show NoS = ""

getS :: Instruction -> S
getS (ADD _ s _ _ _ _) = s
getS (SUB _ s _ _ _ _) = s
getS (RSB _ s _ _ _ _) = s
getS (AND _ s _ _ _ _) = s
getS (ORR _ s _ _ _ _) = s
getS (EOR _ s _ _ _ _) = s
getS (MUL _ s _ _ _)   = s
getS (SMULL _ s _ _ _ _)   = s
getS (SMLAL _ s _ _ _ _)   = s
getS (MLA _ s _ _ _ _)   = s
getS (MOV _ s _ _ _)   = s
getS (MVN _ s _ _ _)   = s

getS CMP{} = S
getS CMN{} = S
getS TST{} = S
getS TEQ{} = S

data UpdateReg = NoUpdate
               | Update
               deriving (Enum,Eq)

instance Show UpdateReg where
    show NoUpdate = ""
    show Update   = "!"

data OffsetDir = Down
               | Up
               deriving (Enum,Eq)

instance Show OffsetDir where
    show Down = "-"
    show Up   = ""


data AddressingModeMain = ImmPreIndex (Argument Register) (Argument Constant) UpdateReg OffsetDir
                        | RegPreIndex (Argument Register) (Argument Register) (ShiftOp Constant) UpdateReg OffsetDir
                        | ImmPostIndex (Argument Register) (Argument Constant) OffsetDir
                        | RegPostIndex (Argument Register) (Argument Register) (ShiftOp Constant) OffsetDir

showImmediateOffset :: Argument Constant -> OffsetDir -> String
showImmediateOffset (ArgC imm) o = "#" ++ show o ++ show imm

instance Show AddressingModeMain where
    show (ImmPreIndex rn imm u o) = "[" ++ show rn ++ ", " ++ showImmediateOffset imm o ++ "]" ++ show u
    show (RegPreIndex rn rm shft u o) = "[" ++ show rn ++ ", " ++ show o ++ show rm ++ shift ++ "]" ++ show u
                    where shift = case shft of
                                    (LSL (ArgC 0)) -> ""
                                    _              -> show shft

    show (ImmPostIndex rn imm o) = "[" ++ show rn ++ "], " ++ showImmediateOffset imm o
    show (RegPostIndex rn rm shft o) = "[" ++ show rn ++ "], " ++ show o ++ show rm ++ shift
                    where shift = case shft of
                                    (LSL (ArgC 0)) -> ""
                                    _              -> show shft

data AddressingModeMulti = IA
                         | IB
                         | DA
                         | DB
                         | EA
                         | FA
                         | ED
                         | FD
                         deriving (Show, Eq, Enum)

data Instruction where
    ADD   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    SUB   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    RSB   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    AND   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    ORR   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    EOR   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    BIC   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    ADC   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    SBC   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    RSC   :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> Instruction
    MUL   :: Condition -> S -> Destination -> Argument Register -> Argument Register -> Instruction
    MLA   :: Condition -> S -> Destination -> Argument Register -> Argument Register -> Argument Register -> Instruction
    SMULL :: Condition -> S -> Destination -> Destination -> Argument Register -> Argument Register -> Instruction
    SMLAL :: Condition -> S -> Destination -> Destination -> Argument Register -> Argument Register -> Instruction
    UMULL :: Condition -> S -> Destination -> Destination -> Argument Register -> Argument Register -> Instruction
    UMLAL :: Condition -> S -> Destination -> Destination -> Argument Register -> Argument Register -> Instruction
    CMP   :: Condition -> Argument Register  -> Argument a -> ShiftOp b -> Instruction
    TST   :: Condition -> Argument Register  -> Argument a -> ShiftOp b -> Instruction
    TEQ   :: Condition -> Argument Register  -> Argument a -> ShiftOp b -> Instruction
    CMN   :: Condition -> Argument Register  -> Argument a -> ShiftOp b -> Instruction
    MOV   :: Condition -> S -> Destination -> Argument a -> ShiftOp b -> Instruction
    MVN   :: Condition -> S -> Destination -> Argument a -> ShiftOp b -> Instruction
    B     :: Condition -> Argument Constant  -> Instruction
    BL    :: Condition -> Argument Constant  -> Instruction
    BX    :: Condition -> Argument Register -> Instruction
    LDR   :: Condition -> Argument Register -> AddressingModeMain -> Instruction
    LDRB  :: Condition -> Argument Register -> AddressingModeMain -> Instruction
    STR   :: Condition -> Argument Register -> AddressingModeMain -> Instruction
    STRB  :: Condition -> Argument Register -> AddressingModeMain -> Instruction
    LDM   :: Condition -> AddressingModeMulti -> Argument Register -> UpdateReg -> [Register] -> Instruction
    STM   :: Condition -> AddressingModeMulti -> Argument Register -> UpdateReg -> [Register] -> Instruction
    HALT  :: Instruction
    JunkInstruction :: Instruction


-- FIXME: This is a pretty horrible hack
instance Eq Instruction where
    x == y = show x == show y

instance Show (Instruction) where
    show (ADD cond s dest src1 src2 shft) = "ADD" ++ stringify3aryOp cond s dest src1 src2 shft
    show (BIC cond s dest src1 src2 shft) = "BIC" ++ stringify3aryOp cond s dest src1 src2 shft
    show (ADC cond s dest src1 src2 shft) = "ADC" ++ stringify3aryOp cond s dest src1 src2 shft
    show (SBC cond s dest src1 src2 shft) = "SBC" ++ stringify3aryOp cond s dest src1 src2 shft
    show (RSC cond s dest src1 src2 shft) = "RSC" ++ stringify3aryOp cond s dest src1 src2 shft
    show (SUB cond s dest src1 src2 shft) = "SUB" ++ stringify3aryOp cond s dest src1 src2 shft
    show (RSB cond s dest src1 src2 shft) = "RSB" ++ stringify3aryOp cond s dest src1 src2 shft
    show (AND cond s dest src1 src2 shft) = "AND" ++ stringify3aryOp cond s dest src1 src2 shft
    show (ORR cond s dest src1 src2 shft) = "ORR" ++ stringify3aryOp cond s dest src1 src2 shft
    show (EOR cond s dest src1 src2 shft) = "EOR" ++ stringify3aryOp cond s dest src1 src2 shft
    show (MUL cond s dest src1 src2) = "MUL" ++ show cond ++ show s ++ " " ++ show dest ++ ", "
                                           ++ show src1 ++ ", " ++ show src2
    show (MLA cond s dest src1 src2 src3) = "MLA" ++ show cond ++ show s ++ " " ++ show dest ++ ", "
                                            ++ show src1 ++ ", " ++ show src2 ++ ", " ++ show src3
    show (SMULL cond s dest1 dest2 src1 src2) = "SMULL" ++ show cond ++ show s ++ " " ++ show dest1 ++ ", "
                                             ++ show dest2 ++ ", " ++ show src1 ++ ", " ++ show src2
    show (SMLAL cond s dest1 dest2 src1 src2) = "SMLAL" ++ show cond ++ show s ++ " " ++ show dest1 ++ ", "
                                             ++ show dest2 ++ ", " ++ show src1 ++ ", " ++ show src2
    show (CMP cond src1 src2 shft) = "CMP" ++ show cond ++ " " ++ show src1 ++ ", " ++ show src2 ++ ", " ++ show shft
    show (CMN cond src1 src2 shft) = "CMN" ++ show cond ++ " " ++ show src1 ++ ", " ++ show src2 ++ ", " ++ show shft
    show (TST cond src1 src2 shft) = "TST" ++ show cond ++ " " ++ show src1 ++ ", " ++ show src2 ++ ", " ++ show shft
    show (TEQ cond src1 src2 shft) = "TEQ" ++ show cond ++ " " ++ show src1 ++ ", " ++ show src2 ++ ", " ++ show shft
    show (MOV cond s dest src1 shft) = "MOV" ++ show cond ++ show s ++ " " ++ show dest
                                             ++ ", " ++ show src1 ++ ", " ++ show shft
    show (MVN cond s dest src1 shft) = "MVN" ++ show cond ++ show s ++ " " ++ show dest
                                             ++ ", " ++ show src1 ++ ", " ++ show shft
    show (B cond src1)             = "B "  ++ show cond ++ " " ++ show src1
    show (BX cond src1)            = "BX " ++ show cond ++ " " ++ show src1
    show (BL cond src1)            = "BL " ++ show cond ++ " " ++ show src1
    show (HALT)               = ""
    show (JunkInstruction)    = "Junk"
    show (LDR cond src dest)  = "LDR" ++ show cond ++ " " ++ show src ++ ", " ++ show dest
    show (LDRB cond src dest) = "LDRB" ++ show cond ++ " " ++ show src ++ ", " ++ show dest
    show (STR cond src dest)  = "STR" ++ show cond ++ " " ++ show src ++ ", " ++ show dest
    show (STRB cond src dest) = "STRB" ++ show cond ++ " " ++ show src ++ ", " ++ show dest
    show (STM cond addrm addr update regs) = "STM" ++ show cond ++ show addrm ++ " " ++ show addr ++ show update ++ ", " ++ "{" ++ stringifyRegList (init regs) ++ show (last regs) ++ "}"
    show (LDM cond addrm addr update regs) = "LDM" ++ show cond ++ show addrm ++ " " ++ show addr ++ show update ++ ", " ++ "{" ++ stringifyRegList (init regs) ++ show (last regs) ++ "}"

stringifyRegList :: [Register] -> String
stringifyRegList = concatMap stringifyRegList'

stringifyRegList' :: Register -> String
stringifyRegList' x = show x ++ ", "

stringify3aryOp :: Condition -> S -> Destination -> Argument Register -> Argument a -> ShiftOp b -> String
stringify3aryOp cond s dest src1 src2 shft = show cond ++ show s ++ " " ++ show dest ++ ", " ++ show src1
                                                       ++ ", " ++ show src2 ++ show shft
