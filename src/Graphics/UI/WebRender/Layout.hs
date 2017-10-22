{-# LANGUAGE TypeFamilies #-}

module Graphics.UI.WebRender.Layout 
  ( Layout
  , layout
  , toLayoutW
  , LayoutW

  -- | a layout which simply draws a solid colour
  , coloured
  -- | a layout which draws a border round its rectangle
  , border
  -- | a layout which draws some text (aligned to the top-left)
  , text'
  -- | a layout which renders some multi-line text according to some alignment
  , text
  , text_
  -- | represents alignment.
  , Align(..)
  -- | draws a layout with a fixed size inside itself
  , box
  -- | centres a layout with a fixed size inside itself
  , centre
  -- | draw two layouts on top of eachother
  , below
  -- | the reverse of 'below'
  , atop
  , ClickEvent(..)
  , onClick
  , Flow
  , defaultSize
  , (/>)
  , (/*>)
  , containing
  , containing_

  , Edge(..)
  , Axis(..)
  
  , flex
  , hflex
  , vflex

  , flow
  , flowt
  , flowb
  , flowl
  , flowr
  ) where

import Graphics.Rendering.WebRender (Rect(..), Colour, Builder, spanWidth, MouseState)
import qualified Graphics.Rendering.WebRender as WR
import qualified Data.Sequence as S
import qualified Data.Vector.Generic as V (toList)
import Data.Monoid (Sum(..))

-- bool is true when event handled
-- false to bubble
newtype ClickEvent = ClickEvent ((Float, Float) -> MouseState -> IO Bool)
withoutEvent :: a -> (ClickEvent, a)
withoutEvent x = (mempty, x)
ignoreEvent :: (ClickEvent, a) -> (ClickEvent, a)
ignoreEvent (_,x) = (mempty, x)
instance Monoid ClickEvent where
  mempty = ClickEvent $ const $ const $ return False
  mappend (ClickEvent e1) (ClickEvent e2) = ClickEvent $ \a b -> do
    handled <- e2 a b
    if handled then return True else e1 a b

-- | The class of all things that can be laid out
class Layout a where
  -- | layout an object into a given rectangle
  layout :: Rect -> a -> (ClickEvent, Builder ())
  toLayoutW :: a -> LayoutW
  toLayoutW = LayoutW

-- | A wrapper for anything which may be laid out
data LayoutW where
  LayoutW :: Layout a => a -> LayoutW

instance Layout LayoutW where
  layout r (LayoutW a) = layout r a
  toLayoutW = id

newtype Blank = Blank ()
blank = Blank ()
instance Layout Blank where
  layout r _ = withoutEvent $ return ()

newtype Coloured = Coloured Colour
coloured = Coloured
instance Layout Coloured where
  layout r (Coloured c) = withoutEvent $ WR.addRect r c

newtype Border = Border (WR.BorderRadius, WR.BorderSides)
border = curry Border
instance Layout Border where
  layout r (Border (br, bs)) = withoutEvent $ WR.addBorder r br bs

newtype Text' = Text' (Colour, String)
text' = curry Text'
instance Layout Text' where
  layout r (Text' (c,s)) = withoutEvent $ WR.addText' r (0,20) c s
newtype Text = Text (Align, Align, Bool, Colour, [String])
text :: Align -> Align -> Bool -> Colour -> String -> Text
text halign valign wrap colour s = Text (halign,valign,wrap,colour,lines s)
text_ :: Align -> Align -> Bool -> Colour -> [String] -> Text
text_ halign valign wrap colour s = Text (halign,valign,wrap,colour,s)
-- function takes height and returns y-offset
wrapText :: Align -> Float -> (Float -> Float) -> String -> Builder (Float, WR.LaidOutText)
wrapText a w f s = do
  shaped <- WR.shapeText s
  let (height,laidOut) = WR.layoutText' layout shaped
      layout vec = let l = V.toList vec
                       lines _ [] [] = []
                       lines _ l [] = [l]
                       lines width l (WR.Glyphs s:xs) = if width + s > w && width /= 0
                         then l : lines s [s] xs else lines (width + s) (l ++ [s]) xs
                       lines width l (WR.Whitespace s:xs) = case xs of
                         [] -> [l]
                         WR.Glyphs s':_ | width + s + s' > w -> lines (width+s) (l++[0]) xs
                         _ -> lines (width+s) (l++[s]) xs
                       lns = lines 0 [] l
                       offset line = let width = sum line
                                         s = case a of 
                                               Start -> 0
                                               End -> w-width
                                               Centre -> (w-width)/2 in 
                                       init $ scanl (+) s line 
                       yoffset = 20 + f (30*fromIntegral (length lns)) in
                     (30*fromIntegral (length lns),
                      concat $ zipWith (\line y -> fmap (\x -> (x,y)) (offset line)) 
                       lns [yoffset,yoffset+30..])
  return (height, laidOut)
instance Layout Text where
  -- for wrapping text vertically aligned to the top, we can give up early
  layout r (Text (ha,Start,True,c,l)) = withoutEvent $ shapelines r l where
    shapelines _ [] = return ()
    shapelines (Rect x y w h) (l:ls) 
      | h <= 0 = return ()
      | True   = do
          (height, laidOut) <- wrapText ha w (const 0) l
          WR.addText (Rect x y w h) c laidOut >> shapelines (Rect x (y+height) w (h-height)) ls
  -- for wrapping text aligned to the bottom we can give up early
  layout r@(Rect _ y w h) (Text (ha,End,True,c,l)) = 
    withoutEvent $ shapelines (h) (reverse l) where
    shapelines _ [] = return ()
    shapelines bot _ | bot <= 0 = return ()
    shapelines bot (l:ls) = do
      (height, laidOut) <- wrapText ha w (bot -) l
      WR.addText r c laidOut >> shapelines (bot-height) ls
      
  -- TODO: centre valign


  -- we layout nonwrapping left-aligned text by puttting lines into flows and
  -- calling layout
  layout r@(Rect _ _ w _) (Text (Start,v,False,c,l)) = ignoreEvent $ case v of
    Start -> layout r $ flowt 30 `containing_` (text' c <$> l)
    End -> layout r $ flowb 30 `containing_` (text' c <$> l)
    Centre -> let h' = 30 * fromIntegral (length l) in
      layout r $ centre (w,h') $ flowt 30 `containing_` (text' c <$> l)
  -- we layout nonwrapping text with other alignment by shaping to determine width and shift
  layout r@(Rect _ _ w' h') (Text (ha,va,False,c,l)) = withoutEvent $ do
    shapedLines <- mapM WR.shapeText l
    let layoutLine baseline s = WR.layoutText (layout baseline) s
        layout b vec = let l = V.toList vec
                           width = sum (spanWidth <$> l)
                           offset = case ha of End -> w' - width; Centre -> (w' - width)/2 in
                         scanl (\(x,y) i -> (x+spanWidth i,y)) (offset,b) l
        n = fromIntegral $ length shapedLines
        offsets' = [20,20+30..]
        offsets = (case va of Start -> id; End -> (h' - 30*n+); Centre -> (0.5*(h'-30*n)+)) <$> offsets'
        lines = zipWith layoutLine offsets shapedLines
    mapM_ (WR.addText r c) lines

data Align = Start -- ^ left or top
           | End -- ^ right or bottom
           | Centre -- ^ centre
data Box = Box (Float, Float) Align Align LayoutW
box :: Layout a => (Float, Float) -> Align -> Align -> a -> Box
box size halign valign x = Box size halign valign (toLayoutW x)
centre :: Layout a => (Float, Float) -> a -> Box
centre (w,h) x = box (w,h) Centre Centre x
instance Layout Box where
  layout (Rect x y w h) (Box (bw,bh) ha va l) =
    let w' = min w bw
        h' = min h bh
        xg = w - w'
        yg = h - h' in
      layout (Rect 
              (case ha of
                 Start -> x
                 End -> x + xg
                 Centre -> x + 0.5*xg)
              (case va of
                 Start -> y
                 End -> y + yg
                 Centre -> y + 0.5*yg)
              w' h') l

data Below where
  Below :: (Layout a, Layout b) => a -> b -> Below
atop, below :: (Layout a, Layout b) => a -> b -> Below
below = Below
atop = flip below
instance Layout Below where
  layout r (Below a b) = layout r a `mappend` layout r b

-- a simple click event
data WithOnClick where
  WithOnClick :: Layout a => a -> (IO ()) -> WithOnClick
onClick :: Layout a => a -> IO () -> WithOnClick
onClick = WithOnClick
instance Layout WithOnClick where
  layout r@(Rect x y w h) (WithOnClick a e) = let (ClickEvent e', b) = layout r a in
    (ClickEvent $ \(mx,my) s ->
        case s of 
          WR.Released WR.LeftB 
            | x <= mx && y <= my && mx <= x+w && my <= y+h -> e >> return True
          _ -> e' (mx,my) s,
     b)


class Flow a where
  type Size a
  infixl 5 />
  infixl 5 /*>
  defaultSize :: a -> Size a
  (/>) :: (Layout b) => a -> b -> a
  a /> b = a /*> (defaultSize a, b)
  (/*>) :: (Layout b) => a -> (Size a, b) -> a
  a /*> b = a `containing` Just b
  containing :: (Layout b, Foldable f) => a -> f (Size a, b) -> a
  containing = foldl (/*>)
  containing_ :: (Layout b, Foldable f) => a -> f b -> a
  containing_ a = foldl (\a x -> a /*> (s,x)) a where s = defaultSize a

data Axis = Horizontal | Vertical
data Flex = Flex Axis (S.Seq (Float, Float, LayoutW)) Float
instance Flow Flex where
  type Size Flex = Float
  defaultSize _ = 1
  Flex a s l /> new = Flex a (s S.|> (l,1,toLayoutW new)) (l+1)
  Flex a s l /*> (w,new) = Flex a (s S.|> (l,w,toLayoutW new)) (l+w)
{- Wrong for now
  Flex a s l `containing` f = Flex a (s S.>< news) (l+w) where
    (news,Sum w) = foldMap (\(s,x) -> (S.singleton $ toLayoutW x,Sum s)) f
  Flex a s l `containing_` f = Flex a (s S.>< news) (l+w) where
    (news,Sum w) = foldMap (\x -> (S.singleton $ toLayoutW x,Sum 1)) f
-}
instance Layout Flex where
  layout (Rect x y w h) (Flex Horizontal elems total) =
    let w' = w / total in
      foldMap (\(start,width,l) -> layout (Rect (x+start*w') y (width*w') h) l)
      elems
  layout (Rect x y w h) (Flex Vertical elems total) =
    let h' = h / total in
      foldMap (\(start,height,l) -> layout (Rect x (y+start*h') w (h'*height)) l)
      elems
flex :: Axis -> Flex
flex a = Flex a S.empty 0
hflex = flex Horizontal
vflex = flex Vertical

data Edge = TopEdge | BottomEdge | LeftEdge | RightEdge
data Flowing = Flowing Edge Float (S.Seq (Float, LayoutW))
instance Flow Flowing where
  type Size Flowing = Float
  defaultSize (Flowing _ w _) = w
  Flowing e w s /> new = Flowing e w (s S.|> (w,toLayoutW new))
  Flowing e w' s /*> (w,new) = Flowing e w' (s S.|> (w,toLayoutW new)) 
instance Layout Flowing where
  layout (Rect x y w h) (Flowing TopEdge _ elems) =
    step (S.viewl elems) y (mempty) where
    end = y+h
    step ((h',l) S.:< s) y b 
      | y < end = 
        step (S.viewl s) (y+h') (b `mappend` layout (Rect x y w h') l)
      | True = b
    step S.EmptyL _ b = b
  layout (Rect x y w h) (Flowing BottomEdge _ elems) =
    step (S.viewr elems) (y+h) (mempty) where
    step (s S.:> (h',l)) y' b 
      | y'+h' > y = 
        step (S.viewr s) (y'-h') (b `mappend` layout (Rect x (y'-h') w h') l)
      | True = b
    step S.EmptyR _ b = b
  layout (Rect x y w h) (Flowing LeftEdge _ elems) =
    step (S.viewl elems) x (mempty) where
    end = x+w
    step ((w',l) S.:< s) x b 
      | x < end = 
        step (S.viewl s) (x+w') (b `mappend` layout (Rect x y w' h) l)
      | True = b
    step S.EmptyL _ b = b
  layout (Rect x y w h) (Flowing RightEdge _ elems) =
    step (S.viewr elems) (x+w) (mempty) where
    step (s S.:> (w',l)) x' b 
      | x'+w' > x = 
        step (S.viewr s) (x'-w') (b `mappend` layout (Rect (x'-w') y w' h) l)
      | True = b
    step S.EmptyR _ b = b
flow :: Edge -> Float -> Flowing
flow alignment defaultSize = Flowing alignment defaultSize S.empty
flowt = flow TopEdge
flowb = flow BottomEdge
flowl = flow LeftEdge
flowr = flow RightEdge
      
