module Machine

where

import Data.Word
import Data.Array.Unboxed
import Data.Array.ST 
import Control.Monad.ST
import qualified Data.IntMap as Map
import Data.IntMap (IntMap, empty)
import Control.Monad

deltaVXPort = 2 :: Word32
deltaVYPort = 3 :: Word32
configPort = 0x3E80 :: Word32

type Addr = Word32 -- 14 bits used
type Val = Double
data PortValue = PortValue !Addr !Val
  deriving (Show, Read, Eq)

type Program = [(OpCode, Val)]
type Input = [PortValue]
type Output = [PortValue]
type Trace = [(Output, Input)]

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
                       , outputs :: UArray Int Val
                       , status :: !Int 
                       , inputs :: IntMap Val }

mkPort :: Word32 -> Double -> PortValue
mkPort = PortValue

memSize = 16384
outputSize = 48 -- Not quite correct.
statusSize = 1

-- | Make an initial machine with the program given.
mkMachine :: Program -> Machine
mkMachine prog = 
    let (instrs, vals) = unzip prog
        dataA = runSTUArray $ do
            let total = memSize + outputSize + statusSize
            newListArray (0, total - 1) 
                         (take total (vals ++ repeat 0))
        dataO = runSTUArray $ do
                  newArray (0, outputSize - 1) 0
    in Machine instrs dataA dataO 0 empty 

-- | Run the machine with the given inputs. Result
-- is the new machine.
exec :: Machine -> Input -> Machine
exec m@(Machine { instrs = instrs }) inps = 
    let m' = m { inputs = readInputs inps }
        newMem = runSTUArray $ do
            mem' <- thaw (dataMem m)
            foldM (step m') mem' (zip instrs [0..])
        outputs = runSTUArray $ do
            -- output ports after memory in array.
            newListArray (0, outputSize) (map (newMem !) [memSize .. memSize + outputSize - 1])
        statusA = runSTUArray $ do
            -- status bit at end of memory
            newListArray (0, 1) [newMem ! (memSize + outputSize)] 
    in m' { dataMem = newMem 
          , outputs = outputs `seq` outputs
          , status = floor (statusA ! 0) }

-- | Read the given output ports
readOutput :: Machine -> [Addr] -> [PortValue]
readOutput (Machine { outputs = outs }) addrs = 
    let find a = PortValue a (outs ! fromEnum a)
    in map find addrs

readInputs :: Input -> IntMap Val
readInputs inps = 
    let toPair :: PortValue -> (Int, Val)
        toPair (PortValue a v) = 
            let i = fromEnum a
                x = (i `seq` i, v)
            in x `seq` x
        inpMap = Map.fromList (map toPair inps)
    in inpMap `seq` inpMap

readDataA mem addr = readArray mem (fromEnum addr)
readOutputA mem addr = readArray mem (memSize + fromEnum addr)
readStatusA mem = readArray mem (memSize + outputSize)

writeDataA mem addr v = writeArray mem (fromEnum addr) (v `seq` v)
writeOutputA mem addr v = writeArray mem (memSize + fromEnum addr) (v `seq` v)
writeStatusA mem v = writeArray mem (memSize + outputSize) (v `seq` v)

-- | Execute a single instruction
step :: Machine -> STUArray s Int Val -> (OpCode, Addr) -> ST s (STUArray s Int Val)
step m mem (Add r1 r2, i) = do
    v1 <- readDataA mem r1
    v2 <- readDataA mem r2
    writeDataA mem i (v1 + v2)
    return $! mem
step m mem (Sub r1 r2, i) = do
    v1 <- readDataA mem r1
    v2 <- readDataA mem r2
    writeDataA mem i (v1 - v2)
    return $! mem
step m mem (Mult r1 r2, i) = do
    v1 <- readDataA mem r1
    v2 <- readDataA mem r2
    writeDataA mem i (v1 * v2)
    return $! mem
step m mem (Div r1 r2, i) = do
    v2 <- readDataA mem r2
    if v2 == 0 
     then writeDataA mem i 0
     else do
       v1 <- readDataA mem r1
       writeDataA mem i (v1 / v2)
    return $! mem
step m mem (Output r1 r2, i) = do
    v1 <- readDataA mem r2
    writeOutputA mem r1 v1
    return $! mem
step m mem (Phi r1 r2, i) = do
    status <- readStatusA mem 
    v <- case floor status of
           1 -> readDataA mem r1
           _ -> readDataA mem r2
    writeDataA mem i v
    return $! mem
step m mem (Cmpz cmp r1, i) = do
    v1 <- readDataA mem r1
    let result = 
            case cmp of
              LTZ -> v1 < 0
              LTEZ -> v1 <= 0
              EQZ -> v1 == 0
              GTEZ -> v1 >= 0
              GTZ -> v1 > 0
    if result 
     then writeStatusA mem 1
     else writeStatusA mem 0
    return $! mem
step m mem (Sqrt r1, i) = do
    v1 <- readDataA mem r1
    writeDataA mem i (sqrt v1)
    return $! mem
step m mem (Copy r1, i) = do
    v1 <- readDataA mem r1
    writeDataA mem i v1
    return $! mem
step (Machine { inputs = inp }) mem (Input r1, i) = do
    let input = case Map.lookup (fromEnum r1) inp of
                  Just v -> v
                  _ -> 0
    writeDataA mem i input
    return $! mem
step m mem (op, addr) = return $ mem