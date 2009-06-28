{-# LANGUAGE PatternSignatures #-}

import System.Environment
import System.IO
import Graphics.Chalkboard
import Graphics.Chalkboard.Viewer
import Control.Concurrent.MVar
import Control.Concurrent
import System.Exit

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
        toR :: Val -> R
        toR = fromRational . toRational
        getPos :: (Output, Input) -> (R, R)
        getPos ([_, _, (PortValue _ px), (PortValue _ py), _],_) = (toR px, toR py)
        config = [WindowSize 200 200
                 , WindowPos 10 10
                 , PixelSize 4
                 , FrameTarget 30]
        factor = 1e-8
        visible c t = if t then alpha c else transparent black
    (t:ts) <- withFile traceF ReadMode $ \h -> do
                getLines h >>= return . map (getPos . read)
    let frame (x, y) = fmap unAlpha $
                       (fmap (visible white) $
                       scale factor $ 
                       circularMaskFor (x * 2, y * 2) (0.01/factor))
                       `over`
                       (fmap (visible blue) $
                       scale factor $ 
                       circularMaskFor (0, 0) (0.03/factor))
    m <- newMVar (frame t)
    let loop [] _ = exitWith ExitSuccess
        loop (f':fs') i = do
         putStrLn $ "Frame " ++ show i
         putMVar m (frame f')
         loop (drop 10 fs') (i + 1)
    forkIO (loop ts 1)
    initBoardViewer config m