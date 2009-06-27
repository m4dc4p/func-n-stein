{-# LANGUAGE PatternSignatures #-}

import System.Environment
import System.IO
import Graphics.Chalkboard
import Graphics.Chalkboard.Viewer
import Control.Concurrent.MVar
import Control.Concurrent

import Machine

main = do
    (traceF:_) <- getArgs
    let getLines h = do
            eof <- hIsEOF h
            if eof 
               then return []
               else do
                 line <- hGetLine h
                 rest <- getLines h
                 return $ line : rest
        getPos :: (Output, Input) -> (Val, Val)
        getPos ([_, _, (PortValue _ px), (PortValue _ py), _],_) = (px, py)
        config = [WindowSize 800 600
                 , WindowPos 10 10
                 , PixelSize 20
                 , FrameTarget 30]
        factor = 10e-8
        visible c t = if t then alpha c else transparent black
    (t:ts) <- withFile traceF ReadMode $ \h -> do
                getLines h >>= return . map (getPos . read)
    let frame (x, y) = fmap unAlpha $
                       fmap (visible white) $
                       circularMaskFor ((fromRational $ toRational x) * factor, (fromRational $ toRational y) * factor) 0.1 
    m <- newMVar (frame t)
    let loop (f':fs') = do
         putMVar m (frame f')
         loop fs'
    forkIO (loop ts)
    initBoardViewer config m