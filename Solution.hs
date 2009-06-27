module Solution 

where

import Data.Word
import Foreign
import System.IO

-- Team ID will always be the same, as will the magic number

magic :: Word32
magic = 0xCAFEBABE
teamID = 98

type ScenarioID = Int

-- | We store only port values because we can derive the count of ports
-- from the length of the list. We don't store timestep because we can
-- derive that from the order of elements in the list given to Solution.
type Frame = [PortValue]

type Addr = Int
type Value = Double 
data PortValue = PortValue Addr Value

-- | Our solution indicates the scenario and the frames making up
-- our trace. Frames come in order, from timestep 0 to end.
data Solution = Solution ScenarioID [Frame]

writeSolution file (Solution scenario frames) = 
    withBinaryFile file WriteMode $ \h -> do
      let hPutWord :: Word32 -> IO ()
          hPutWord w = with w $ \p -> do
              hPutBuf h p (sizeOf w)
      hPutWord magic
      hPutWord (int2Word teamID)
      hPutWord (int2Word scenario)
      let writePort :: PortValue -> IO ()
          writePort (PortValue addr val) = do
              -- Get binary representation of double as Word64
              v <- allocaBytes 8 $ \p -> do 
                       poke p val 
                       mapM (\i -> peekElemOff (castPtr p) i) [0, 1]
              -- write to file
              hPutWord (int2Word addr)
              hPutWord (v !! 0)
              hPutWord (v !! 1)
          writeFrame :: Int -> Frame -> IO ()
          writeFrame step ports = do
              hPutWord (int2Word step)
              hPutWord (int2Word . length $ ports)
              mapM_ writePort ports
          finalFrame = []
      mapM_ (uncurry writeFrame) (zip [0..] (frames ++ [finalFrame]))

word2Char :: Word32 -> Char
word2Char = toEnum . fromEnum

int2Word :: Int -> Word32
int2Word = toEnum
