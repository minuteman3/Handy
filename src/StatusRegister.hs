module Handy.CPSR where

import Data.Bits
import Data.Int (Int32)

data CPSR = CPSR { negative :: Bool
                 , zero     :: Bool
                 , carry    :: Bool
                 , overflow :: Bool
                 } deriving Show

toConstant :: CPSR -> Int32
toConstant cpsr = n .|. z .|. c .|. v
                where n = if negative cpsr then bitDefault 31 else 0
                      z = if zero cpsr then bitDefault 30 else 0
                      c = if carry cpsr then bitDefault 29 else 0
                      v = if overflow cpsr then bitDefault 28 else 0

fromConstant :: Int32 -> CPSR
fromConstant val = CPSR { negative = testBitDefault val 31
                        , zero     = testBitDefault val 30
                        , carry    = testBitDefault val 29
                        , overflow = testBitDefault val 28
                        }
