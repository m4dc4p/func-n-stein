{-# LANGUAGE BangPatterns #-}
module Hohmann

where

import Data.Array.Unboxed
import Data.Maybe
import Debug.Trace
import System.IO
import System.IO.Unsafe

import Machine
import Scenario
import Vector

data HohmannState = HS Elapsed [Position] Step
type Elapsed = Int
type Position = Vector2
data Step = Start 
          | Inner TargetRadius FinalSpeed
          | Done 
type StartRadius = Double
type TargetRadius = Double
type TransitSpeed = Double
type FinalSpeed = Double
type Fired = Bool

tr msg v = 
    let io = unsafePerformIO $ do
               putStrLn msg
               hFlush stdout
    in io `seq` v

hohmannInit = HS 0 [] Start

-- | Read outputs for hohmann scenario.
hohmannOuts :: Extractor
hohmannOuts m = readOutput m [0x0, 0x1, 0x2, 0x3, 0x4]

-- | The controller that runs the hohmann scenario
hohmann :: Controller HohmannState
hohmann hs@(HS !elapsed !pos Start) m 
    | elapsed < (fromIntegral . floor $ period) = (initHS hs (currPos m), [])
    | otherwise =
        let p = currPos m
            startRadius = magnitude p
            targetRadius = let r = (outputs m) ! 0x4 in r `seq` r
            atxI = 1 / ((startRadius + targetRadius) / 2)
            viA = sqrt (earthGM / startRadius)
            vfB = sqrt (earthGM / targetRadius)
            vtxA = sqrt (earthGM * (2 / startRadius - atxI))
            vtxB = sqrt (earthGM * (2 / targetRadius - atxI))
            dvA = vtxA - viA
            dvB = vfB - vtxB
            velocity = currVelocity (reflect p) (reflect $ last pos) period
            thrust = dvA `scale` normalize velocity
        in (stepHS hs p (Inner targetRadius dvB)
           , deltaV thrust)
hohmann hs@(HS _ !pos s@(Inner targetRadius dvB)) m 
    | abs (currRadius - targetRadius) < 100 = 
          (stepHS hs p Done, deltaV thrust)
    | otherwise = (stepHS hs p s, [])
  where
    p = currPos m 
    velocity = currVelocity p (last pos) period
    currSpeed = magnitude velocity
    currRadius = magnitude p
    thrust = dvB `scale` normalize velocity
{-hohmann hs@(HS _ !pos Done) m = tr ("distance: " ++ show (abs $ currRadius - targetRadius)) (hs, [])
  where
    p = currPos m 
    currRadius = magnitude p
    targetRadius = let r = (outputs m) ! 0x4 in r `seq` r-}
hohmann hs m = (hs, [])

initHS :: HohmannState -> Position -> HohmannState
initHS (HS !elapsed !pos _) p = HS (elapsed + 1) (p : pos) Start

stepHS :: HohmannState -> Position -> Step -> HohmannState
stepHS (HS !elapsed !pos _) p s = HS (elapsed + 1) (p : init pos) s

currVelocity p' p period =  (1 / period) `scale` (p' `vecSub` p)

fuel :: Double
fuel = 10000

period :: Double
period = 1 -- measure velocity over 10 seconds
        
hohmannStop :: HohmannState -> Machine -> Bool
hohmannStop _ m = currScore m /= 0

