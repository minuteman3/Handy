module Handy.StatusRegister where

import Data.Bits
import Data.Int (Int32)

data StatusRegister = StatusRegister { negative :: Bool
                                     , zero     :: Bool
                                     , carry    :: Bool
                                     , overflow :: Bool
                                     } deriving Show

blankStatusRegister :: StatusRegister
blankStatusRegister = StatusRegister { negative = False
                                     , zero     = False
                                     , carry    = False
                                     , overflow = False
                                     }

toConstant :: StatusRegister -> Int32
toConstant cpsr = n .|. z .|. c .|. v
                where n = if negative cpsr then bit 31 else 0
                      z = if zero cpsr then bit 30 else 0
                      c = if carry cpsr then bit 29 else 0
                      v = if overflow cpsr then bit 28 else 0

fromConstant :: Int32 -> StatusRegister
fromConstant val = StatusRegister { negative = testBit val 31
                                  , zero     = testBit val 30
                                  , carry    = testBit val 29
                                  , overflow = testBit val 28
                                  }
