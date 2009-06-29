{-# LANGUAGE BangPatterns #-}
module Vector 

where

newtype Vector2 = V2 (Double, Double)
    deriving (Eq)

instance Show Vector2 where
    show = showVector

showVector (V2 (x, y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

infixl 6 `vecAdd`
infixl 7 `scale`

vecAdd :: Vector2 -> Vector2 -> Vector2
vecAdd (V2 (!x1, !y1)) (V2 (!x2, !y2)) = 
    let x1' = x1 + x2
        y1' = y1 + y2 
    in V2 (x1' `seq` x1', y1' `seq` y1')

vecSub :: Vector2 -> Vector2 -> Vector2
vecSub (V2 (!x1, !y1)) (V2 (!x2, !y2)) = 
    let x1' = x1 - x2
        y1' = y1 - y2 
    in V2 (x1' `seq` x1', y1' `seq` y1')

scale :: Double -> Vector2 -> Vector2
scale s (V2 (!x1, !y1)) = 
    let x1' = x1 * s
        y1' = y1 * s
    in V2 (x1' `seq` x1', y1' `seq` y1')

vector2 :: Double -> Double -> Vector2
vector2 x y = V2 (x `seq` x, y `seq` y)

vecX (V2 (!x, !y)) = x
vecY (V2 (!x, !y)) = y

normalize :: Vector2 -> Vector2
normalize v@(V2 (!x, !y)) = 
    let x' = (x / mag)
        y' = (y / mag)
    in V2 (x' `seq` x', y' `seq` y')
    where
      mag = magnitude v

magnitude :: Vector2 -> Double
magnitude (V2 (!x, !y)) = 
    let m = sqrt $ x**2 + y**2
    in m `seq` m
