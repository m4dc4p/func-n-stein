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
        getPos :: (Output, Input) -> (R, R, R)
        getPos ([_, _, (PortValue _ px), (PortValue _ py), (PortValue _ targetRadius)],_) = 
            (toR px, toR py, toR targetRadius)
        config = [WindowSize 200 200
                 , WindowPos 10 10
                 , PixelSize 4
                 , FrameTarget 30]
        factor = 1e-8
        visible c t = if t then alpha c else transparent black
    (t:ts) <- withFile traceF ReadMode $ \h -> do
                getLines h >>= return . map (getPos . read)
    let (_, _, targetRadius) = t
        frame (x, y, _) = fmap unAlpha $
                       -- our satellite
                       (fmap (visible red) $
                       scale factor $ 
                       circularMaskFor (x, y) (0.01/factor))
                       `over`
                       -- earth
                       (fmap (visible blue) $
                       scale factor $ 
                       circularMaskFor (0, 0) (6.357e6)) 
                       `over`
                       -- target radius
                       (fmap (visible white) $ 
                        scale factor $
                        circularMaskFor (0,0) (targetRadius))
    m <- newMVar (frame t)
    let loop [] i = loop (t:ts) 0
        loop (f':fs') i = do
         putStrLn $ "Frame " ++ show i
         putMVar m (frame f')
         loop (drop 10 fs') (i + 1)
    forkIO (loop ts 1)
    initBoardViewer config m