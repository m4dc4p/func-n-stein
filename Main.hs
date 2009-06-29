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
import Network.CGI.Protocol (maybeRead)

import Machine
import Solution
import ReadObf
import Scenario
import Hohmann

data Scenario = Hohmann
              | MeetAndGreet 
              | Eccentric
              | Empty
              | Single
              | Simple
              | Echo
              | DoNothing
  deriving (Eq, Show, Read)

data Options = Scenario Scenario
            | Config Int
            | Help 
  deriving (Eq, Show, Read)

opts = [Option ['s'] ["scenario"] (ReqArg scenario "scenario") "Scenario to use: hohmann, meetAndGreet, eccentric, empty, single, simple, echo or nothing."
       , Option ['c'] ["config"] (ReqArg config "config") "Configuration of scenario to run."
       , Option ['?'] ["help"] (NoArg Help) "Help text." ]

scenario arg = 
    Scenario $ case map toLower arg of
      "hohmann" -> Hohmann
      "meetAndGreet" -> MeetAndGreet
      "eccentric" -> Eccentric
      "empty" -> Empty
      "single" -> Single
      "simple" -> Simple
      "echo" -> Echo
      "nothing" -> DoNothing
      _ -> error $ "Unrecognized scenario: " ++ arg

config which = case maybeRead which of
                 Just i -> Config i
                 Nothing -> error $ "Can't read config: " ++ which

findScenario as = listToMaybe [s | (Scenario s) <- as]
findHelp as = listToMaybe [h | h@Help <- as]
findConfig as = listToMaybe [s | (Config s) <- as]

main = do
  (args, _, err) <- getArgs >>= return . getOpt RequireOrder opts 
  when (isJust (findHelp args)) $ do
         putStrLn $ usageInfo "Usage: main [options]" opts
         exitWith ExitSuccess
  let scenarioID = maybe (error $ "Scenario ID not supplied")
                         id
                         (findScenario args)
      obf = case scenarioID of
              Hohmann -> "bin1.obf"
              MeetAndGreet -> "bin2.obf"
              Eccentric -> "bin3.obf"
              Empty -> "bin1.obf"
              Single -> "bin1.obf"
              Simple -> "bin1.obf"
              Echo -> "bin1.obf"
              DoNothing -> "bin1.obf"
      configID = maybe 1001
                     id
                     (findConfig args)
      baseName = (map toLower (show scenarioID)) ++ "_" ++ show configID 
      obfDump = baseName ++ ".dmp"
      obfRaw = baseName ++ ".raw"
      programName = baseName ++ ".prg"
      traceFile = baseName ++ ".trc"
      fileName = baseName ++ ".osf"
      hohmannIDs = ["1001", "1002", "1003", "1004"]
      meetAndGreetIDs = ["2001", "2002", "2003", "2004"]
      eccentricIDs = ["3001", "3002", "3003", "3004"]
  instrs <- withBinaryFile obf ReadMode readObf
  writeRawFile obf obfRaw
  writeInstructionsOnly obfDump instrs
  let program = instrsToProgram instrs
  writeProgram programName program
  let result = runScenario scenarioID configID program
  writeTrace traceFile result
  writeSolution fileName (Solution configID (map snd result))

-- | Run a specific scenario.
runScenario Empty _ _ = emptySolution
runScenario Single _ _ = singleSolution
runScenario Simple _ _ = simpleSolution
runScenario Echo configID program = runMachine (echoOutput hohmannOuts) neverStop hohmannOuts hohmannInit configID program
runScenario DoNothing configID program = runMachine nothing neverStop hohmannOuts hohmannInit configID program
runScenario Hohmann configID program = runMachine hohmann hohmannStop hohmannOuts hohmannInit configID program
runScenario scenarioID configID program = error $ "Unsupported scenario " ++ show scenarioID

-- | Write a trace of the output for a scenario.
writeTrace traceFile trace = withFile traceFile WriteMode $ \h -> do
    mapM_ (hPutStrLn h . show) trace

writeProgram programName program = withFile programName WriteMode $ \h -> do
    mapM_ (hPutStrLn h . show) program

writeInstructionsOnly obfDump instrs = withFile obfDump WriteMode $ \h -> do
    let showInstr (Instr i val) = do
            ([v1, v2] :: [Word32]) <- with val $ \p -> do
                v1 <- peekElemOff (castPtr p) 0
                v2 <- peekElemOff (castPtr p) 1
                return [v1, v2]
            hPutStrLn h $ showHex (opDMask i) (" : " ++ showHex i (" : " ++ showHex (revWord i) (", (" ++ showHex v1 (", " ++ showHex v2 ")"))))
    mapM_ showInstr instrs


writeRawFile obf obfRaw = withBinaryFile obf ReadMode $ \f -> do
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

-- A solution with no inputs
emptySolution :: Trace
emptySolution = []

-- A solution with one input
singleSolution :: Trace
singleSolution = [([], [mkPort configPort 1001])
                 ,([], [PortValue deltaVXPort (100.100)
                       ,PortValue deltaVYPort (-100.100)])]

-- A solution with some random inputs
simpleSolution :: Trace
simpleSolution = [([], [mkPort configPort 1001])
                 ,([], [PortValue deltaVXPort (100.100)
                       , PortValue deltaVYPort (-100.100)])
                 ,([], [PortValue deltaVXPort 0])]

-- Takes all double values found in instructions and
-- echoes them back to an input port.
echoSolution :: [Instr] -> Trace
echoSolution instrs = 
    let ports = map (\(Instr _ v) -> [PortValue deltaVXPort v]) instrs
    in ([], [mkPort configPort 1001]) :
       map (\p -> ([], p)) ports