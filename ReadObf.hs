module ReadObf

where

import Data.Word
import Foreign
import System.IO
import Control.Monad

data Instr = Instr Word32 Double

type Executable = [Instr]

readObf :: Handle -> IO Executable
readObf f = do
    size <- hFileSize f >>= return . fromEnum
    allocaBytes size $ \p -> do
        _ <- hGetBuf f p size
        let toInstrE i = do
                -- With even addresses, instruction comes first.
                instr <- getWord p i 
                val <- getDouble p (i + 1)
                return $ Instr instr val
            toInstrO i = do
                -- With odd addresses, value comes first.
                val <- getDouble p i
                instr <- getWord p (i + 2)
                return $ Instr instr val
            getWord p i = do
                v <- peekElemOff (castPtr p) i
                return $ (v :: Word32)
            getDouble p i = do
                v <- peekElemOff (castPtr p) i
                return $ (v :: Double)
            applyE i instrs = do
                is <- toInstrE i
                return (is : instrs)
            applyO i instrs = do
                is <- toInstrO i
                return (is : instrs)
        instrs <- foldM (\is (f, i) -> f i is) [] (zip (cycle [applyE, applyO]) [0,3..size])
        return $ reverse instrs
