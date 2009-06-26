
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import Data.Char
import Machine
import Solution

data Options = Scenario String
            | Filename String
            | Help 
  deriving Eq

opts = [Option ['s'] ["scenario"] (ReqArg Scenario "scenario") "Scenario ID to use (empty and single are special)."
       , Option ['f'] ["filename"] (ReqArg Filename "filename") "File to write solution to."
       , Option ['?'] ["help"] (NoArg Help) "Help text." ]

findScenario as = listToMaybe [s | (Scenario s) <- as]
findFilename as = listToMaybe [f | (Filename f) <- as]

main = do
  (args, _, err) <- getArgs >>= return . getOpt RequireOrder opts 
  let scenarioID = maybe (error $ "Scenario ID not supplied")
                         (map toLower)
                         (findScenario args)
      fileName = maybe "out.txt"
                       id
                       (findFilename args)
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
