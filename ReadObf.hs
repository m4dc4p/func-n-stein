module ReadObf

where

import Data.Word
import Foreign
import System.IO
import Control.Monad

import Machine

data Instr = Instr Word32 Double
  deriving (Show, Read, Eq)

type Executable = [Instr]

readObf :: Handle -> IO Executable
readObf f = do
    size <- hFileSize f >>= return . fromEnum
    allocaBytes size $ \p -> do
        _ <- hGetBuf f p size
        let toInstrE i = do
                -- With even addresses, instruction comes first.
                instr <- getVal p i 
                val <- getVal p (i + 4)
                return $ Instr instr val
            toInstrO i = do
                -- With odd addresses, value comes first.
                val <- getVal p i
                instr <- getVal p (i + 8)
                return $ Instr instr val
            getVal p i = peekByteOff p i 
            applyE i instrs = do
                is <- toInstrE i
                return (is : instrs)
            applyO i instrs = do
                is <- toInstrO i
                return (is : instrs)
        instrs <- foldM (\is (f, i) -> f i is) [] (zip (cycle [applyE, applyO]) [0,12..size])
        return $ reverse instrs

instrsToProgram :: Executable -> Program
instrsToProgram = map instrToOp

-- | Mask to obtain D-type opcode
opDMask :: Word32 -> Word32
opDMask v = (v .&. 0xF)

-- | Mask for D-type R1 address
r1DMask :: Word32 -> Word32
r1DMask v = (v .&. 0x3FFF0) `shiftR` 4

-- | Mask for D-type R2 address
r2DMask :: Word32 -> Word32
r2DMask v = (v .&. 0x3FFF) `shiftR` 18

-- | Mask for S-type opcode
opSMask :: Word32 -> Word32
opSMask v = (v .&. 0xF0) `shiftR` 4

-- | Mask for S-type immediate opcode
immSMask :: Word32 -> Word32
immSMask v = (v .&. 0x700) `shiftR` 8

-- | Mask for S-type R1 address
r1SMask :: Word32 -> Word32
r1SMask v = (v .&. 0xFFFC0000) `shiftR` 18

instrToOp :: Instr -> (OpCode, Val)
instrToOp (Instr instr val) 
    | isDOp instr = 
        let opD = opDMask r
            r1 = r1DMask r
            r2 = r2DMask r
        in (mkDOp opD r1 r2, val)
    | otherwise = 
        let opS = opSMask r
            imm = immSMask r
            r1 = r1SMask r
        in (mkSOp opS imm r1, val)
    where
      isDOp instr = instr .&. 0xF0000000 /= 0
      r = revWord instr

mkDOp :: Word32 -> Word32 -> Word32 -> OpCode
mkDOp op r1 r2 = 
    case op of
      0x1 -> Add r1 r2
      0x2 -> Sub r1 r2
      0x3 -> Mult r1 r2
      0x4 -> Div r1 r2
      0x5 -> Output r1 r2
      0x6 -> Phi r1 r2
      _ -> error $ "Unrecognized D-type op: " ++ show op

mkSOp :: Word32 -> Word32 -> Word32 -> OpCode
mkSOp op imm r1 = 
    case op of
      0x0 -> Noop
      0x1 -> 
          let mkImmOp 0x0 = LTZ
              mkImmOp 0x1 = LTEZ
              mkImmOp 0x2 = EQZ
              mkImmOp 0x3 = GTEZ
              mkImmOp 0x4 = GTZ
              mkImmOp op = error $ "Unrecognized immediate: " ++ show op
          in Cmpz (mkImmOp imm) r1
      0x2 -> Sqrt r1
      0x3 -> Copy r1
      0x4 -> Input r1
      _ -> error $ "Unrecognized S-type op: " ++ show op

-- | From http://graphics.stanford.edu/~seander/bithacks.html#BitReverseObvious
revWord :: Word32 -> Word32
revWord word = 
          let v1 = ((word `shiftR` 1) .&. 0x55555555) .|. ((word .&. 0x55555555) `shiftL` 1) -- // swap odd and even bits
              v2 = ((v1 `shiftR` 2) .&. 0x33333333) .|. ((v1 .&. 0x33333333) `shiftL` 2); -- // swap consecutive pairs
              v3 = ((v2 `shiftR` 4) .&. 0x0F0F0F0F) .|. ((v2 .&. 0x0F0F0F0F) `shiftL` 4); -- // swap nibbles ... 
              v4 = ((v3 `shiftR` 8) .&. 0x00FF00FF) .|. ((v3 .&. 0x00FF00FF) `shiftL` 8); -- // swap bytes
              v5 = ( v4 `shiftR` 16             ) .|. ( v4               `shiftL` 16); -- // swap 2-byte long pairs
          in v5
