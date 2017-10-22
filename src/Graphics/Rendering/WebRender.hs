{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Rendering.WebRender 
  ( newWindow
  , Builder
  , MouseButton(..)
  , MouseState(..)
  , Rect(Rect)
  , Colour(Colour)
  , BorderStyle(..)
  , BorderSide(..)
  , BorderSides(..)
  , simpleBorder
  , BorderRadius(..)
  , square
  , rounded
  , rounded'
  , addRect
  , addBorder
  , addText'

  , ShapedRunInfo(..)
  , ShapedRunPosition
  , ShapedText
  , LaidOutText
  , shapeText
  , layoutText
  , layoutText'
  , addText

  , black
  , red
  , green
  , blue
  , white
  ) where

import Control.Concurrent.STM
import Data.Word (Word32)
import Data.Int (Int32)
import Foreign.C.String (CString(..), newCString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Marshal.Alloc (free,alloca)
import Foreign.Storable
import Foreign.ForeignPtr
import qualified Foreign.Concurrent as FC -- allows Haskell finalizers
import Foreign.Ptr
import qualified Data.Vector.Storable as V
import Data.Bits (testBit)
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Rendering.WebRender.BorderStyle (BorderStyle)
import qualified Graphics.Rendering.WebRender.BorderStyle as BS

data DisplayListBuilder_s
data ShapedText_s
type ShapedTextS = Ptr ShapedText_s
type DisplayListBuilder = Ptr DisplayListBuilder_s
data ShapedRunInfo = Glyphs {spanWidth :: Float}
                   | Whitespace {spanWidth :: Float} deriving Show
type ShapedRunInfoS = Ptr ShapedRunInfo
type ShapedRunPosition = (Float, Float) -- ShapedRunPosition Float Float deriving Show
newtype ShapedRunPosition' = ShapedRunPosition' (Float, Float) deriving Show
type ShapedRunPositionS = Ptr ShapedRunPosition'

instance Storable ShapedRunInfo where
  sizeOf _ = 8
  alignment _ = 8
  -- { width: f32, is_whitespace: u32 }
  peek p = do
    construct <$> (toBool <$> peekByteOff p 4) <*> peekByteOff p 0 where
      toBool :: Word32 -> Bool
      toBool = (/=0)
      construct True = Whitespace
      construct False = Glyphs
  poke p (Glyphs width) = pokeByteOff p 0 width >> pokeByteOff p 4 (0 :: Word32)
  poke p (Whitespace width) = pokeByteOff p 0 width >> pokeByteOff p 4 (1 :: Word32)
instance Storable ShapedRunPosition' where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ShapedRunPosition' <$> ((,) <$> peekByteOff p 0 <*> peekByteOff p 4)
  poke p (ShapedRunPosition' (x,y)) = pokeByteOff p 0 x >> pokeByteOff p 4 y
{-
instance Storable ShapedRunPosition where
  sizeOf _ = 8
  alignment _ = 8
  peek p = do
    ShapedRunPosition <$> peekByteOff p 0 <*> peekByteOff p 4
  poke p (ShapedRunPosition x y) = 
    pokeByteOff p 0 x >> pokeByteOff p 4 y
-}

foreign import ccall "wrapper"
  wrapRender :: (DisplayListBuilder -> Float -> Float -> IO ()) ->
    IO (FunPtr (DisplayListBuilder -> Float -> Float -> IO ()))
foreign import ccall "wrapper"
  wrapMouse :: (Float -> Float -> Int32 -> IO ()) -> 
    IO (FunPtr (Float -> Float -> Int32 -> IO ()))
foreign import ccall safe "new_window"
  new_window :: CString -> Word32 -> Word32
    -> FunPtr (DisplayListBuilder -> Float -> Float -> IO ())
    -> FunPtr (Float -> Float -> Int32 -> IO ()) -> IO ()
foreign import ccall unsafe "display_list_builder_push_rect" 
  display_list_builder_push_rect :: DisplayListBuilder -> Float -> Float -> Float -> Float
    -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "display_list_builder_push_border_n"
  display_list_builder_push_border_n :: DisplayListBuilder ->
    Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> 
    Float -> Float -> Float -> Float -> CInt ->
    Float -> Float -> Float -> Float -> CInt ->
    Float -> Float -> Float -> Float -> CInt ->
    Float -> Float -> Float -> Float -> CInt ->
    Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "display_list_builder_push_text"
  display_list_builder_push_text :: DisplayListBuilder ->
    Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float ->
    CString -> IO ()
foreign import ccall unsafe "display_list_builder_shape_text"
  display_list_builder_shape_text :: DisplayListBuilder -> CString -> IO ShapedTextS
foreign import ccall unsafe "shaped_text_free"
  shaped_text_free :: ShapedTextS -> IO ()
foreign import ccall unsafe "&shaped_text_free"
  shaped_text_free' :: FunPtr (ShapedTextS -> IO ())
foreign import ccall unsafe "shaped_text_get_widths"
  shaped_text_get_widths :: ShapedTextS -> Ptr CSize -> IO ShapedRunInfoS
foreign import ccall unsafe "shaped_text_widths_free"
  shaped_text_widths_free :: ShapedRunInfoS -> CSize -> IO ()
foreign import ccall unsafe "display_list_builder_push_shaped_text"
  display_list_builder_push_shaped_text :: DisplayListBuilder ->
    Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> 
    ShapedTextS -> ShapedRunPositionS -> CSize -> IO ()

-- | This represents a sequence of instructions to build a display
-- list. Later objects are drawn on top of earlier ones
newtype Builder a = Builder { runBuilder :: DisplayListBuilder -> IO a }

instance Monoid a => Monoid (Builder a) where
  mempty = Builder $ const $ return mempty
  Builder a `mappend` Builder b = Builder $ \x -> mappend <$> a x <*> b x

instance Functor Builder where
  fmap f (Builder a) = Builder $ \x -> f <$> a x

instance Applicative Builder where
  pure = Builder . const . pure
  Builder a <*> Builder b = Builder $ \x -> a x <*> b x

instance Monad Builder where
  return = pure
  Builder a >>= f = Builder $ \x -> runBuilder <$> (f <$> a x) >>= ($x)

data MouseButton = LeftB | RightB | MiddleB 
                 | Other Int deriving (Ord,Eq,Show,Read)
button 1 = LeftB
button 2 = RightB
button 3 = MiddleB
button n = Other (fromIntegral n-4)
data MouseState = Pressed MouseButton | Released MouseButton | Moved deriving (Ord,Eq,Show,Read)

newWindow :: Integral a => String -> (a,a) -> 
             ((Float, Float) -> IO (Builder ())) ->
             ((Float, Float) -> MouseState -> IO ()) -> IO ()
newWindow title (height,width) render mouse = do
  ctitle <- newCString title
  mouseTV <- newTVarIO (0,0)
  let (h,w) = (fromIntegral height, fromIntegral width)
  rfp <- wrapRender $
    \x w h -> runBuilder <$> (render (w,h)) <*> pure x >>= id
  mfp <- wrapMouse $
    \x y n -> do
      p <- if x<= -99999 && y<= -99999 
           then readTVarIO mouseTV
           else atomically (writeTVar mouseTV (x,y)) >> (return (x,y))
      mouse p (case compare n 0 of
                  LT -> Released $ button $ -n
                  EQ -> Moved
                  GT -> Pressed  $ button $  n)
  new_window ctitle h w rfp mfp
  free ctitle

-- | A Rectangle specified by the (x,y) coordinates of its upper left
-- corner, and its width and height.
data Rect = Rect {x :: Float, y :: Float, w :: Float, h :: Float }
rect :: Rect -> (Float -> Float -> Float -> Float -> a) -> a
rect (Rect x y w h) f = f x y w h
-- | A Colour in the RGBA space. All parameters should be between 0
-- and 1 (inclusive).
data Colour = Colour {r :: Float, g :: Float, b :: Float, a :: Float }
colour :: Colour -> (Float -> Float -> Float -> Float -> a) -> a
colour (Colour r g b a) f = f r g b a

-- | The specification for one line in a border: Its width, colour, and style.
data BorderSide = BorderSide Float Colour BorderStyle
-- | The specification for all the sides in a border
data BorderSides = BorderSides {left :: BorderSide, top :: BorderSide, 
                                right :: BorderSide, bottom :: BorderSide }
-- | The specification for how to round the corners in a radius
data BorderRadius = BorderRadius { top_left :: (Float, Float), top_right :: (Float, Float),
                                   bottom_left :: (Float, Float), bottom_right :: (Float, Float) }

square :: BorderRadius
square = rounded 0 -- ^ A 'BorderRadius' where all corners are square
rounded :: Float -> BorderRadius
rounded r = rounded' 15 r -- ^ A 'BorderRadius' where all corners have the same radius
-- | A 'BorderRadius' where all corners are either square or rounded
-- with a given radius The first argument specifies which corners are
-- to be rounded via the bits which are set:
--   * 1: Top left corner
--   * 2: Top right corner
--   * 4: Bottom left corner
--   * 8: Bottom right corner
rounded' :: Int -> Float -> BorderRadius
rounded' n r = BorderRadius (t 0) (t 1) (t 2) (t 3) where
  t i = if testBit n i then (r,r) else (0,0)

-- | Creates a 'BorderSides' where each border has the same style as specified by Width, Style and Colour.
simpleBorder :: Float -> BorderStyle -> Colour -> BorderSides
simpleBorder w s c = BorderSides side side side side where
  side = BorderSide w c s

-- | Draw a rectangle in a specified colour
addRect :: Rect -> Colour -> Builder ()
addRect r c = Builder $ \x -> colour c $ rect r $ display_list_builder_push_rect x

-- | Draw a border for a specified (rounded) rectangle with styles specified for each side.
addBorder :: Rect -> BorderRadius -> BorderSides -> Builder ()
addBorder r br bs = Builder $ \x ->
  radii br $ sides $ widths $ rect r $ display_list_builder_push_border_n x where
  widths f = 
    let BorderSides (BorderSide l _ _) (BorderSide t _ _) (BorderSide r _ _) (BorderSide b _ _) = bs in
      f l t r b
  sides f = side (bottom bs) $ side (right bs) $ side (top bs) $ side (left bs) $ f
  side (BorderSide _ c s) f = (colour c $ f) (BS.toCInt s)
  radii (BorderRadius (tlw,tlh) (trw,trh) (blw,blh) (brw,brh)) f = f tlw tlh trw trh blw blh brw brh

-- | Draw some text inside a rectangle. The text is given the (local)
-- position of the first character (with the y-coordinate being the baseline).
addText' :: Rect -> (Float, Float) -> Colour -> String -> Builder ()
{-
addText' r@(Rect x y _ _) (ox, oy) c text = Builder $ \d -> do
  str <- newCString text
  (colour c $ rect r $ display_list_builder_push_text d) (x+ox) (y+oy) str
  free str
-}
addText' r (ox,oy) c text = do
  shaped <- shapeText text
  let layout = layoutText lay shaped
  addText r c layout
  where
    lay info = init $ scanl (\(ox,oy) x ->
                          let w = (case x of Whitespace w -> w; Glyphs w -> w)
                          in (ox+w, oy))
               (ox,oy) $ V.toList info

shapeWidthsFree :: Ptr ShapedRunInfo -> CSize -> IO ()
shapeWidthsFree p len = shaped_text_widths_free p len
data ShapedText = ShapedText (ForeignPtr ShapedText_s) (V.Vector ShapedRunInfo) deriving (Show)
shapeText :: String -> Builder ShapedText
shapeText text = Builder $ \d -> do
  str <- newCString text
  st <- display_list_builder_shape_text d str
  stfp <- newForeignPtr shaped_text_free' st
  free str
  -- now get the widths
  alloca $
    \p -> do
      shapedRun <- shaped_text_get_widths st p
      len <- peek p
      fp <- FC.newForeignPtr shapedRun (shapeWidthsFree shapedRun len)
      let vec = V.unsafeFromForeignPtr0 fp (fromIntegral len)
      return $ ShapedText stfp vec

data LaidOutText = LaidOutText (ForeignPtr ShapedText_s) (V.Vector ShapedRunPosition') deriving (Show)
layoutText :: (V.Vector ShapedRunInfo -> [ShapedRunPosition]) -> ShapedText -> LaidOutText
layoutText f (ShapedText s info) = LaidOutText s $
  V.fromListN (V.length info) (ShapedRunPosition' <$> f info)
layoutText' :: Functor f => (V.Vector ShapedRunInfo -> f [ShapedRunPosition]) -> ShapedText -> f LaidOutText
layoutText' f (ShapedText s info) = LaidOutText s <$>
  V.fromListN (V.length info) <$> fmap ShapedRunPosition' <$> f info

-- | Add some text which has been laid out. The positions should be
-- relative to the top left corner of 'rect' and position the baseline
-- at the left-edge of each slice
addText :: Rect -> Colour -> LaidOutText -> Builder ()
addText r c (LaidOutText s v) = Builder $ \d -> withForeignPtr s 
                                                $ \s -> V.unsafeWith v $ \v' -> do
  (colour c $ rect r $ display_list_builder_push_shaped_text d) s v' (fromIntegral $ V.length v)

-- | Some simple colours
black, red, green, blue, white :: Colour
black = Colour 0 0 0 1
red =   Colour 1 0 0 1
green = Colour 0 1 0 1
blue =  Colour 0 0 1 1
white = Colour 1 1 1 1

