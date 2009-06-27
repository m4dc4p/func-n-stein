module Machine

where

import Data.Word
import Data.Array.Unboxed

deltaVXPort = 2 :: Int 
deltaVYPort = 3 :: Int
configPort = 0x3E80 :: Int

type Addr = Word32 -- 14 bits used
type Val = Double

type Program = [(OpCode, Val)]

-- | Machine op codes
data OpCode = Add Addr Addr
            | Sub Addr Addr
            | Mult Addr Addr
            | Div Addr Addr
            | Output Addr Addr
            | Phi Addr Addr
            | Noop
            | Cmpz Op Addr
            | Sqrt Addr
            | Copy Addr
            | Input Addr
  deriving (Show, Read, Eq)

-- | Operands for Cmpz
data Op = LTZ
    | LTEZ
    | EQZ
    | GTEZ    
    | GTZ
  deriving (Show, Read, Eq)

data Machine = Machine { instrs :: [OpCode]
                       , dataMem :: UArray Int Val
                       , status :: !Int 
                       , inputs :: UArray Int Val
                       , outputs :: UArray Int Val }

