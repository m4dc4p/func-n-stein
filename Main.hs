
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import Data.Char
import System.IO

import Machine
import Solution
import ReadObf

data Options = Scenario String
            | Executable String
            | Filename String
            | Help 
  deriving Eq

opts = [Option ['s'] ["scenario"] (ReqArg Scenario "scenario") "Scenario ID to use (empty and single are special)."
       , Option ['f'] ["filename"] (ReqArg Filename "filename") "File to write solution to."
       , Option ['e'] ["executable"] (ReqArg Executable "executable") "OBF file to read."
       , Option ['?'] ["help"] (NoArg Help) "Help text." ]

findScenario as = listToMaybe [s | (Scenario s) <- as]
findFilename as = listToMaybe [f | (Filename f) <- as]
findObf as = listToMaybe [f | (Executable f) <- as]

main = do
  (args, _, err) <- getArgs >>= return . getOpt RequireOrder opts 
  let scenarioID = maybe (error $ "Scenario ID not supplied")
                         (map toLower)
                         (findScenario args)
      fileName = maybe "out.txt"
                       id
                       (findFilename args)
      obf = maybe (error $ "OBF file must be supplied.")
                  id
                  (findObf args)
  instrs <- withBinaryFile obf ReadMode readObf
  -- mapM_ (\(Instr i v) -> putStrLn $ show i ++ " : " ++ show v) instrs
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
