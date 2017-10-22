{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.Rendering.WebRender as WR
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
render :: WordVec -> (Float, Float) -> IO (Builder ())
render v (w,h) = do
  words <- intercalate " " . take 800 . randomWords v <$> getGen
  return $ snd $ layout (Rect 0 0 w h) $ coloured white `below`
    text Start Start True black words

chunk n l = let (s,r) = splitAt n l in case r of
  [] -> [s]
  x -> s : chunk n x
renderfew :: Align -> Align -> Bool -> WordVec -> (Float, Float) -> IO (Builder ())
renderfew a1 a2 b v (w,h) = do
  lines <- fmap (intercalate " ") . chunk 10 . take 100 . randomWords v <$> getGen
  return $ snd $ layout (Rect 0 0 w h) $ coloured white `below`
    text_ a1 a2 b black lines

renderfns :: WordVec -> [(Float,Float) -> IO (Builder ())]
renderfns v = cycle $ render v : ([renderfew a b False v | a <- [Start, Centre, End], b <- [Start, Centre, End]] ++
                                  [renderfew a b True  v | a <- [Start, Centre, End], b <- [Start, End]])
renderAll :: TVar ((Float, Float) -> IO (Builder ())) -> (Float, Float) -> IO (Builder ())
renderAll v x = do render <- readTVarIO v
                   render x

swapper :: WordVec -> IO (TVar ((Float, Float) -> IO (Builder ())), (Float, Float) -> MouseState -> IO ())
swapper v = do
  let x1:xs = renderfns v
  v <- newTVarIO x1
  v' <- newTVarIO xs
  return (v, \_ p -> case p of 
                       Released LeftB -> atomically $ do
                         (x:xs) <- readTVar v'
                         writeTVar v' xs
                         writeTVar v  x
                       _ -> return ())
main = do
  args <- getArgs
  let file = case args of [x] -> x; _ -> "/usr/share/dict/words";
  words <- loadWords file
  (var, handle) <- swapper words
  newWindow "words" (200,200) (renderAll var) handle
