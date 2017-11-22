{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.Rendering.WebRender as WR hiding (render)
import Graphics.UI.WebRender.Layout as L
import Data.List
import qualified Data.Vector as V'
import qualified Data.Vector.Generic as V
import System.Environment (getArgs)
import System.Random
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.STM

type WordVec = V'.Vector String

loadWords :: FilePath -> IO WordVec
loadWords p = V.fromList . words <$> readFile p

randomWords :: RandomGen g => WordVec -> g -> [String]
randomWords w g = (w V.!) <$> randomRs (0,V.length w - 1) g

getGen = newStdGen
--getGen = getStdGen

padding = 10
base = 20
gap = 30
render :: WordVec -> (FontFaceDescription,FontSize) -> IO (Builder ())
render v (d,s) = do
  words <- intercalate " " . take 800 . randomWords v <$> getGen
  return $ do
    (w,h) <- getSize
    snd $ layout (Rect 0 0 w h) $ coloured white `below`
      text Start Start True d s black words

chunk n l = let (s,r) = splitAt n l in case r of
  [] -> [s]
  x -> s : chunk n x
renderfew :: Align -> Align -> Bool -> WordVec -> (FontFaceDescription,FontSize) -> IO (Builder ())
renderfew a1 a2 b v (d,s) = do
  lines <- fmap (intercalate " ") . chunk 10 . take 100 . randomWords v <$> getGen
  return $ do
    (w,h) <- getSize
    snd $ layout (Rect 0 0 w h) $ coloured white `below`
      text_ a1 a2 b d s black lines

renderfns :: WordVec -> (FontFaceDescription,FontSize) -> [IO (Builder ())]
renderfns v f = cycle $ render v f : 
  ([renderfew a b False v f | a <- [Start, Centre, End], b <- [Start, Centre, End]] ++
   [renderfew a b True  v f | a <- [Start, Centre, End], b <- [Start, End]])
renderAll :: TVar (IO (Builder ())) -> IO (Builder ())
renderAll v = do x <- readTVarIO v
                 x

swapper :: WordVec -> (FontFaceDescription,FontSize) -> IO (TVar (IO (Builder ())), IO ())
swapper v f = do
  let x1:xs = renderfns v f
  v <- newTVarIO x1
  v' <- newTVarIO xs
  return (v, atomically $ do
    (x:xs) <- readTVar v'
    writeTVar v' xs
    writeTVar v  x)

font = (FontFace "DejaVu Sans" Italic normalWeight NormalStretch, 20)

main = do
  args <- getArgs
  let file = case args of [x] -> x; _ -> "/usr/share/dict/words";
  words <- loadWords file
  (var, swp) <- swapper words font
  win <- newWindow "words" (200,200)
  let
    handle (x:xs) | "(MOUSEINPUT RELEASED LEFT" `isPrefixOf` x = swp >> handle xs
                  | x == "CLOSED" || x == "(KEYBOARDINPUT PRESSED 9 ESCAPE)" = closeWindow win
                  | otherwise = handle xs
    handle [] = loop
    loop = do
      r <- renderAll var
      (_,events) <- renderGetEvents win r
      handle events
  loop

