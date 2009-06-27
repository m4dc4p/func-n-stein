{-# LANGUAGE PatternSignatures #-}
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import Data.Char
import System.IO
import System.FilePath
import System.Exit
import Control.Monad
import Debug.Trace
import Foreign
import Numeric

import Machine
import Solution
import ReadObf

data Options = Scenario String
            | Executable String
            | Help 
  deriving Eq

opts = [Option ['s'] ["scenario"] (ReqArg Scenario "scenario") "Scenario ID to use (empty and single are special)."
       , Option ['e'] ["executable"] (ReqArg Executable "executable") "OBF file to read."
       , Option ['?'] ["help"] (NoArg Help) "Help text." ]

findScenario as = listToMaybe [s | (Scenario s) <- as]
findHelp as = listToMaybe [h | h@Help <- as]
findObf as = listToMaybe [f | (Executable f) <- as]

main = do
  (args, _, err) <- getArgs >>= return . getOpt RequireOrder opts 
  when (isJust (findHelp args)) $ do
         putStrLn $ usageInfo "Usage: main [options]" opts
         exitWith ExitSuccess
  let scenarioID = maybe (error $ "Scenario ID not supplied")
                         (map toLower)
                         (findScenario args)
      obf = maybe (error $ "OBF file must be supplied.")
                  id
                  (findObf args)
      baseName = takeBaseName obf ++ "_" ++ scenarioID 
      obfDump = baseName ++ ".dmp"
      obfRaw = baseName ++ ".raw"
      programName = baseName ++ ".prg"
      fileName = baseName ++ ".sol"
  instrs <- withBinaryFile obf ReadMode readObf
  withBinaryFile obf ReadMode $ \f -> do
      size <- hFileSize f >>= return . fromEnum
      allocaBytes size $ \p -> do
          _ <- hGetBuf f p size
          withBinaryFile obfRaw WriteMode $ \h -> do
              let writeInstrRaw (f, i1) = do
                      -- hPutBuf h (p `plusPtr` (f i1)) 4
                      v1 :: Word32 <- peekByteOff p (f i1)
                      hPutStrLn h (showHex (opDMask v1) (" : " ++ showHex v1 "") )
                  wE i = i + 8
                  wO i = i
              mapM_ writeInstrRaw (zip (cycle [wE, wO]) [0,12..size])
  -- write raw instructions
  withFile obfDump WriteMode $ \h -> do
      let showInstr (Instr i val) = do
              ([v1, v2] :: [Word32]) <- with val $ \p -> do
                  v1 <- peekElemOff (castPtr p) 0
                  v2 <- peekElemOff (castPtr p) 1
                  return [v1, v2]
              hPutStrLn h $ showHex (opDMask i) (" : " ++ showHex i (" : " ++ showHex (revWord i) (", (" ++ showHex v1 (", " ++ showHex v2 ")"))))
      mapM_ showInstr instrs

  withFile programName WriteMode $ \h -> do
      mapM_ (hPutStrLn h . show) $ instrsToProgram instrs
  
  let result = case scenarioID of
          "empty" -> emptySolution
          "single" -> singleSolution
          "simple" -> simpleSolution
          _ -> error $ "Unrecognized scenarioID " ++ scenarioID
  writeSolution fileName result


-- A solution with no inputs
emptySolution = Solution 1001 []

-- A solution with one input
singleSolution = Solution 1001 [[PortValue configPort 1001]
                               ,[PortValue deltaVXPort (100.100)
                                ,PortValue deltaVYPort (-100.100)]]

-- A solution with one input
simpleSolution = Solution 1001 [[PortValue configPort 1001]
                            ,[PortValue deltaVXPort (100.100)
                             ,PortValue deltaVYPort (-100.100)]
                            ,[PortValue deltaVXPort 0]]
