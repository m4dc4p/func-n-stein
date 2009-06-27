{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Scenarios 

where

import Data.List

import Machine
import Solution

type Controller s = s -> Machine -> (s, Input)
type Tester s = s -> Machine -> Bool
type Extractor = Machine -> Output

-- | The controller that does nothing.
nothing :: Controller s
nothing x _ = (x, [])

-- | The controller that reads the output adn
-- feeds it into the inputs.
echoOutput :: Extractor -> Controller s
echoOutput extract x m = 
    let [PortValue _ v1, PortValue _ v2] = take 2 (extract m)
    in (x, [PortValue 0x1 v1, PortValue 0x2 v2])

-- | The test that runs the simulation forever
neverStop :: Tester s
neverStop _ _ = False

-- | Read outputs for hohmann scenario.
hohmannOuts :: Extractor
hohmannOuts m = readOutput m [0x0, 0x1, 0x2, 0x3, 0x4]

-- | The controller that runs the hohmann scenario
hohmann :: Controller Int
hohmann = nothing

hohmannStop = neverStop

data Go s = Go !Input s !Machine !Int

runMachine :: Controller s
           -> Tester s
           -> Extractor
           -> s -> ScenarioID -> Program -> Trace
runMachine sim done getOutps init scenario prog =
    let maxIterations = 10000
        step (Go inps s m !cnt) 
            | cnt > maxIterations = Nothing
            | otherwise = 
                let m' = exec m inps
                    (s', inps') = sim s m'
                    outps' = getOutps m'
                in Just ((outps', inps), Go inps' s' m' (cnt + 1))
        config = [mkPort configPort (fromRational . toRational $ scenario)]
        machine = mkMachine prog
     in unfoldr step (Go config init machine 0)