{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.Rendering.WebRender as WR

import qualified Data.Map.Strict as M
import System.Clock
import System.Random
import Control.Concurrent
import Control.Concurrent.STM
import System.Environment (getArgs)
import Data.Array.IO
import Data.Array.MArray
import Data.List (isPrefixOf)

-- Wilson's algorithm
data Point = P Int Int deriving (Show, Eq, Ord)
data Board = Board Point Point deriving Eq -- size of board
data State = State Board -- ^ size of board
             (M.Map Point Point) -- ^ each point has one neighbour closer to the root than it. This defines that relationship
             [Point] -- ^ the current path being trialled. The first element is most recently explored
instance Eq State where
  State a b c == State a' b' c' = a==a' && c==c' && b==b'
board :: Point -> Point -> Board
board (P x y) (P x' y') = Board (P (min x x') (min y y')) (P (max x x') (max y y'))
neighbours :: Point -> [Point]
neighbours (P x y) = [P (x-1) y, P x (y-1), P (x+1) y, P x (y+1)]
neighbour :: Point -> Point -> Bool
neighbour (P x y) (P x' y') = diff xd yd || diff yd xd where
  xd = x - x'
  yd = y - y'
  diff a b = abs a == 1 && b == 0

on :: Point -> Board -> Bool
on (P x y) (Board (P xn yn) (P xx yx)) = xn <= x && x <= xx &&
                                     yn <= y && y <= yx


-- | behaves like member in CL
member' :: Eq a => a -> [a] -> [a]
member' x [] = []
member' x (y:ys) | x == y    = y:ys
                 | otherwise = member' x ys

stepPoint :: State -> Point -> (State, Bool)
stepPoint (State b m []) p | M.member p m = (State b m [], False)
                           | otherwise    = (State b m [p], True)
stepPoint (State b m ps@(p:_)) p' 
  | not (p `neighbour` p') || not (p' `on` b) = (State b m ps, False)
  | not (M.member p' m) = case p' `member'` ps of
      [] -> (State b m (p':ps), True) -- p' is not in ps
      ps' -> (State b m ps', True) -- cut loop
  | otherwise = (State b (M.union m (pathInTree ps p)) [], True) where
      pathInTree ps p = M.fromList $ zip ps (p':ps)
newPoint :: State -> IO (Maybe Point)
newPoint (State _ _ (p:p':_)) = Just <$> (ns!!) <$> getStdRandom (randomR (0,length ns - 1)) where
  ns = filter (/=p') $ neighbours p
newPoint (State _ _ (p:_)) = Just <$> (ns!!) <$> getStdRandom (randomR (0,length ns - 1)) where
  ns = neighbours p
newPoint (State b m _) = let Board (P xn yn) (P xx yx) = b
                             missing = [P x y | y <- [yn..yx], x <- [xn..xx], not (M.member (P x y) m)]
                             n = length missing - 1 
                             p = P <$> getStdRandom (randomR (xn,xx)) <*> getStdRandom (randomR (yn,yx)) in 
  case missing of 
    (x:_) -> return (Just x)
    []    -> return (Nothing)
{-  do
    p1 <- p
    if M.member p1 m then return $ Just p1
      else do p2 <- p
              if M.member p2 m then return $ Just p2
                else if n == 0 then return Nothing
                     else Just <$> (missing !!) <$> getStdRandom (randomR (0,n))
-}

solved :: State -> Bool
solved (State (Board (P xn yn) (P xx yx)) m []) = not $ any (not . flip M.member m) [P x y | x <- [xn..xx], y <- [yn..yx]]
solved _ = False



drawState :: (Int, Int) -> State -> Builder ()
drawState (w,h) (State (Board (P xn yn) (P xx yx)) m ps) = drawTree baseCol m >> drawPath pathCol ps where
  baseCol = Colour 1 1 1 1
  pathCol = Colour 1 0 1 1
  pointWidth, pointHeight :: Float
  pointWidth = pointSize w xn xx
  pointHeight = pointSize h yn yx
  pointXSep = pointWidth + 1
  pointYSep = pointHeight + 1
  pointSize s n x = fromIntegral (s - (x - n)) / fromIntegral (x - n + 1)
  ox x = fromIntegral (x - xn)
  oy y = fromIntegral (y - yn)
  -- | given a point and the one before it in the tree, draw the point
  drawPoint :: Colour -> Point -> Point -> Builder ()
  drawPoint c (P x y) (P x' y') = addRect (Rect px py pw ph) c where
    px = pointXSep * (ox x) - if x' == x - 1 then 1 else 0
    py = pointYSep * (oy y) - if y' == y - 1 then 1 else 0
    pw = if abs (x' - x) == 1 then pointXSep else pointWidth
    ph = if abs (y' - y) == 1 then pointYSep else pointHeight
  drawTree :: Colour -> M.Map Point Point -> Builder ()
  drawTree c = M.foldMapWithKey (drawPoint c)
  drawPath :: Colour -> [Point] -> Builder ()
  drawPath c []     = return ()
  drawPath c (p:ps) = sequence_ $ zipWith (drawPoint c) (p:ps) (p:p:ps)

stepState :: Int -> State -> IO (State, Bool, Bool)
stepState n s = ss' n s False where
  ss' n s c | n <= 0 = return (s,c, solved s)
            | True   = do
                p <- newPoint s
                case p of
                  Nothing -> return (s,c,True)
                  Just p -> let (s',c') = stepPoint s p in
                              ss' (n-1) s' (c || c')

mazeThread :: TVar State -> IO ()
mazeThread stateVar = readTVarIO stateVar >>= mazeThread' where
  mazeThread' state = do
    (state', changed, solved) <- stepState 10 state
    if changed then atomically $ writeTVar stateVar state' else return ()
    if changed then threadDelay 500 else return ()
    if solved
      then let wait = do threadDelay 8333
                         state'' <- readTVarIO stateVar
                         if state'' /= state' then return () else wait
           in wait >> readTVarIO stateVar >>= mazeThread'
      else mazeThread' state'

window n l = case splitAt n l of
  (a,[]) -> a : []
  (a,b)  -> a : window n b

mean x = sum x / fromIntegral (length x)

drawFps :: (MArray a Float m) => Font -> Builder Rect -> a Int Float -> m (Builder ())
drawFps f b arr = do
  bounds <- getBounds arr
  let col = Colour 0 1 1 0.8
  dat <- sequence [mean <$> mapM (readArray arr) x | x <- window 4 $ range bounds]
  return $ do
    (Rect x y w h) <- b
    let width = 4 * w / fromIntegral (rangeSize bounds)
    addRect (Rect x y w h) (Colour 0.5 0.5 0.5 0.7)
    sequence_ [addRect (Rect (x + i * width) (y+h-height) width height) col 
              | (fps,i) <- zip dat [1..],
                let height = (min h (h * fps / 60))]
    let fps = fromIntegral (round (last dat * 10)) / 10 :: Float
    addText' (Rect x y w h) (w/2-30,h-10) f red (show fps ++ "fps")

updateFps :: MArray a Float m => a Int Float -> Float -> m ()
updateFps arr f = do
  r <- range <$> getBounds arr
  sequence_ [readArray arr i >>= writeArray arr j | (i,j) <- zip (tail r) r]
  writeArray arr (last r) f

data AfterEvent = NextFrame | Wait | NextFrame' | Close
instance Monoid AfterEvent where
  mempty = NextFrame
  Close `mappend` _ = Close
  _ `mappend` Close = Close
  NextFrame' `mappend` _  = NextFrame'
  _ `mappend` NextFrame'  = NextFrame'
  Wait `mappend` _  = Wait
  _ `mappend` Wait  = Wait
  _ `mappend` _     = NextFrame

main :: IO ()
main = do
  let size = (999,999)
  args <- getArgs
  let width = case args of
        [x] -> read x
        _   -> 100
  let name = "Wilson's algorithm " ++ show width ++ "×" ++ show width
  let b = board (P 1 1) (P width width)
  let start = P (width `div` 2) (width `div` 2)
  win <- newWindow name size
  stateVar <- newTVarIO $ State b (M.singleton start start) []
  forkIO $ mazeThread stateVar
  fpsHistory <- newArray (1,1200) 0 :: IO (IOArray Int Float)
  time <- newTVarIO $ Nothing
  (font,_) <- renderGetEvents win $ addRect (Rect 0 0 2000 2000) black >>
    getFont (FontFace "DejaVu Sans" Upright normalWeight NormalStretch) 20
  let handleEvent solved ev = if "(MOUSEINPUT RELEASED LEFT" `isPrefixOf` ev
        then (atomically $ writeTVar stateVar $
             State b (M.singleton start start) [])
             >> return NextFrame'
        else if ev == "CLOSED" || ev == "(KEYBOARDINPUT PRESSED 9 ESCAPE)"
        then return Close
        else if "(RESIZED" `isPrefixOf` ev
        then return NextFrame'
        else do
        if solved
          then return Wait
          else return NextFrame
  let doAction _ NextFrame  = loop
      doAction _ NextFrame' = loop
      doAction _ Close      = closeWindow win
      doAction s Wait       = waitEvent win >>= handleEvent s >>= doAction s
      loop = do
        ctime <- getTime Monotonic
        frameTime <- atomically $ do
          t <- readTVar time
          let TimeSpec s ns = ctime
          writeTVar time (Just ctime)
          return $ case t of Nothing -> 1; Just (TimeSpec s' ns') -> fromIntegral (s-s') + (fromIntegral (ns-ns') * 1e-9)
        let fps = 1/frameTime
        updateFps fpsHistory fps
        fpsDraw <- drawFps font ((\w -> Rect (w/2-150) 0 300 30) <$> getWidth) fpsHistory
        state <- readTVarIO stateVar
        (_,events) <- renderGetEvents win $ do
          (w,h) <- getSize
          addRect (Rect 0 0 w h) black
          drawState (floor w, floor h) state
          fpsDraw
        let s = solved state
        action <- mconcat <$> mapM (handleEvent s) events
        doAction s action
  loop

