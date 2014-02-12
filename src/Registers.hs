module Handy.Registers (
    get,
    set,
    blankRegisterFile,
    RegisterFile,
    Register(R0,
             R1,
             R2,
             R3,
             R4,
             R5,
             R6,
             R7,
             R8,
             R9,
             R10,
             R11,
             R12,
             R13,
             R14,
             R15,
             SP,
             LR,
             PC,
             None
             )
) where

import Data.Int (Int32)

data Register = R0
              | R1
              | R2
              | R3
              | R4
              | R5
              | R6
              | R7
              | R8
              | R9
              | R10
              | R11
              | R12
              | R13
              | R14
              | R15
              | SP
              | LR
              | PC
              | None
              deriving (Show, Eq, Enum)

data RegisterFile = RegisterFile {
    _r0  :: Int32,
    _r1  :: Int32,
    _r2  :: Int32,
    _r3  :: Int32,
    _r4  :: Int32,
    _r5  :: Int32,
    _r6  :: Int32,
    _r7  :: Int32,
    _r8  :: Int32,
    _r9  :: Int32,
    _r10 :: Int32,
    _r11 :: Int32,
    _r12 :: Int32,
    _r13 :: Int32,
    _r14 :: Int32,
    _r15 :: Int32
} deriving (Show)

blankRegisterFile :: RegisterFile
blankRegisterFile = RegisterFile { _r0=0
                                 , _r1=0
                                 , _r2=0
                                 , _r3=0
                                 , _r4=0
                                 , _r5=0
                                 , _r6=0
                                 , _r7=0
                                 , _r8=0
                                 , _r9=0
                                 , _r10=0
                                 , _r11=0
                                 , _r12=0
                                 , _r13=0
                                 , _r14=0
                                 , _r15=0
                                 }


get :: RegisterFile -> Register -> Int32
get rf r = case r of
             R0  -> _r0 rf
             R1  -> _r1 rf
             R2  -> _r2 rf
             R3  -> _r3 rf
             R4  -> _r4 rf
             R5  -> _r5 rf
             R6  -> _r6 rf
             R7  -> _r7 rf
             R8  -> _r8 rf
             R9  -> _r9 rf
             R10 -> _r10 rf
             R11 -> _r11 rf
             R12 -> _r12 rf
             R13 -> _r13 rf
             R14 -> _r14 rf
             R15 -> _r15 rf
             SP  -> _r13 rf
             LR  -> _r14 rf
             PC  -> _r15 rf
             None -> 0

set :: RegisterFile -> Register -> Int32 -> RegisterFile
set rf r v = case r of
                 R0  -> rf {_r0=v}
                 R1  -> rf {_r1=v}
                 R2  -> rf {_r2=v}
                 R3  -> rf {_r3=v}
                 R4  -> rf {_r4=v}
                 R5  -> rf {_r5=v}
                 R6  -> rf {_r6=v}
                 R7  -> rf {_r7=v}
                 R8  -> rf {_r8=v}
                 R9  -> rf {_r9=v}
                 R10 -> rf {_r10=v}
                 R11 -> rf {_r11=v}
                 R12 -> rf {_r12=v}
                 R13 -> rf {_r13=v}
                 R14 -> rf {_r14=v}
                 R15 -> rf {_r15=v}
                 SP  -> rf {_r13=v}
                 LR  -> rf {_r14=v}
                 PC  -> rf {_r15=v}
                 None -> rf
