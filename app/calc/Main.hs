module Main where

import Graphics.Rendering.WebRender as WR
import Graphics.UI.WebRender.Layout

import Control.Concurrent.STM

type Stack = [Float]
data State = State Stack (Maybe String)

finishInput :: State -> State
finishInput s@(State _ Nothing) = s
finishInput (State s (Just input)) = State (read input : s) Nothing

apInput' :: Char -> String -> State -> State
apInput' c def (State s (Just i)) = State s (Just (i ++ [c]))
apInput' c def (State s Nothing)  = State s (Just def)
apInput c = apInput' c [c]

pop :: State -> Either State (Float, State)
pop s = pop' (finishInput s) where
  pop' s@(State [] _) = Left s
  pop' (State (x:xs) y) = Right (x, State xs y)

pop2 :: State -> Either State ((Float, Float), State)
pop2 s = do
  (x2,s) <- pop s
  (x1,s) <- pop s
  return ((x1,x2),s)

push :: Float -> State -> State
push x (State s i) = State (x:s) i
push' :: Either State (Float, State) -> State
push' (Left x) = x
push' (Right (x,s)) = push x s

op1 f s = push' $ do
  (x,s) <- pop s
  return (f x, s)
op2 f s = push' $ do
  ((x1,x2),s) <- pop2 s
  return (f x1 x2, s)

opb1 s f  = Button s (op1 f)
opb2 s f  = Button s (op2 f)
numb c    = Button [c] (apInput c)
numb' c s = Button [c] (apInput' c s)

butnegve = Button "+/-" neg where
  neg (State s (Just i)) = case (head i) of
    '-' -> State s (Just (tail i))
    _   -> State s (Just ('-':i))
  neg (State [] Nothing) = State [] Nothing
  neg (State (s:ss) Nothing) = State ((-s) : ss) Nothing

butback = Button "←" $ \state ->
  case state of
    State [] Nothing -> State [] Nothing
    State (s:ss) Nothing -> State ss Nothing
    State s (Just [c]) -> State s Nothing
    State s (Just x) -> State s (Just $ init x)

-- push or duplicate
dup s@(State _ (Just _)) = finishInput s
dup (State [] Nothing) = State [] Nothing
dup (State (s:ss) Nothing) = State (s:s:ss) Nothing

swap s = swap' (finishInput s) where
  swap' (State (s1:s2:ss) _) = State (s2:s1:ss) Nothing
  swap' s = s

data Button = Button String (State -> State)
buttons :: [[Button]]
buttons =
  [[Button "↑" dup, Button "swap" swap, butback]
  ,[numb '7', numb '8',       numb '9', opb1 "exp" exp, opb1 "log" log]
  ,[numb '4', numb '5',       numb '6', opb2 "+" (+),   opb2 "−" (-)]
  ,[numb '1', numb '2',       numb '3', opb2 "×" (*),   opb2 "÷" (/)]
  ,[numb '0', numb' '.' "0.", butnegve, opb2 "^" (**),  opb1 "sqrt" sqrt]]

drawButton sv (Button s op) = 
  text Centre Centre False black s `onClick` 
  (atomically $ do s <- readTVar sv
                   writeTVar sv (op s))

drawButtons sv =
  below (coloured (Colour 0.75 0.75 0.75 1.0)) $ 
  vflex `containing_` 
  (fmap (containing_ hflex . fmap (drawButton sv)) buttons)

drawState (State st input) = 
  below (coloured cream) $ withInput $ flowb 40 `containing_` stack where
  cream = Colour 0.9 0.85 0.8 1.0
  st' = reverse (zip [1..] st)
  stack = si <$> st'
  si (pos, num) = text Start Centre False black (show pos ++ ":") `below` text End Centre False black (show num)
  withInput x = case input of
    Nothing -> x
    Just i -> x /> text End Centre False black i

render sv ev (w,h) = do
  s <- readTVarIO sv
  let l = vflex /*> (3, drawState s) /*> (4, drawButtons sv)
  let (e,b) = layout (Rect 0 0 w h) (coloured red `below` l)
  atomically $ writeTVar ev e
  return b

event ev a b = do
  ClickEvent e <- readTVarIO ev
  e a b
  return ()

noEvent = ClickEvent $ \ a b -> return False

main = do
  sv <- newTVarIO (State [] Nothing)
  ev <- newTVarIO noEvent
  newWindow "calc" (300,600) (render sv ev) (event ev)
