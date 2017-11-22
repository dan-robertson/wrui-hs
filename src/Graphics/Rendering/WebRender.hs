{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Rendering.WebRender 
  ( newWindow
  , closeWindow
  , Event
  , pollEvent
  , waitEvent
  , clearEvents
  , Builder
  , render
  , renderGetEvents
  , getSize
  , getWidth
  , getHeight
  , MouseButton(..)
  , MouseState(..)
  , Rect(Rect)
  , Colour(Colour)

  , addRect

  , BorderStyle(..)
  , BorderSide(..)
  , BorderSides(..)
  , simpleBorder
  , BorderRadius(..)
  , square
  , rounded
  , rounded'
  , addBorder

  , Font
  , FontFamily
  , FontSize
  , Italicness(..)
  , Weight
  , normalWeight
  , bold
  , weight
  , Stretchiness(..)
  , FontFaceDescription(..)
  , getFont

  , addText'

  , ShapedRunInfo(..)
  , ShapedRunPosition
  , ShapedText
  , LaidOutText
  , shapeText
  , shapeText'
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
import Data.Word (Word32, Word16, Word8)
import Data.Int (Int32)
import Foreign.C.String (CString(..), newCString, peekCString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Marshal.Alloc (free,alloca)
import Foreign.Storable
import Foreign.ForeignPtr
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Foreign.Concurrent as FC -- allows Haskell finalizers
import Foreign.Ptr
import qualified Data.Vector.Storable as V
import Data.Bits (testBit)
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Rendering.WebRender.BorderStyle (BorderStyle)
import qualified Graphics.Rendering.WebRender.BorderStyle as BS

data Window_s
newtype Window = Window (ForeignPtr Window_s, IORef Bool) -- bool is true if window is open
type WindowP = Ptr Window_s
data DisplayListBuilder_s
type DisplayListBuilder = Ptr DisplayListBuilder_s
data Font_s
type FontP = Ptr Font_s
type Font = ForeignPtr Font_s
data ShapedText_s
type ShapedTextS = Ptr ShapedText_s
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

foreign import ccall safe "wrui_new_window"
  new_window :: CString -> Word32 -> Word32 -> IO WindowP
foreign import ccall safe "&wrui_close_window"
  close_window :: FunPtr (WindowP -> IO ())
foreign import ccall unsafe "wrui_get_event_sexp"
  get_event_sexp :: WindowP -> Int32 -> IO CString -- Bool goes as Int32. 0 is false.
foreign import ccall unsafe "wrui_free_event"
  free_event :: CString -> IO ()
foreign import ccall unsafe "wrui_display_list_builder"
  display_list_builder :: WindowP -> Ptr Float -> Ptr Float -> IO DisplayListBuilder
foreign import ccall unsafe "display_list_builder_free"
  display_list_builder_free :: DisplayListBuilder -> IO ()
foreign import ccall unsafe "display_list_builder_build"
  display_list_builder_build :: WindowP -> DisplayListBuilder -> IO ()
-- TODO: clip stuff
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
foreign import ccall unsafe "wrui_find_font"
  find_font :: DisplayListBuilder -> CString -> Float -> Int32 {- Bool -} ->
  Word16 {- weight -} -> Word8 {- stretch -} -> IO FontP
foreign import ccall unsafe "&wrui_free_font"
  free_font :: FunPtr (FontP -> IO ())
foreign import ccall unsafe "display_list_builder_push_text"
  display_list_builder_push_text :: DisplayListBuilder ->
    Float -> Float -> Float -> Float -> FontP ->
    Float -> Float -> Float -> Float -> Float -> Float ->
    CString -> IO ()
foreign import ccall unsafe "wrui_shape_text"
  shape_text :: FontP -> CString -> IO ShapedTextS
foreign import ccall unsafe "wrui_free_shaped_text"
  shaped_text_free :: ShapedTextS -> IO ()
foreign import ccall unsafe "&wrui_free_shaped_text"
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
newtype Builder a = Builder { runBuilder :: (DisplayListBuilder,Float,Float) -> IO a }

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

getSize :: Builder (Float, Float)
getSize = Builder $ \(_,w,h) -> pure (w,h)
getWidth = fst <$> getSize
getHeight = snd <$> getSize

data MouseButton = LeftB | RightB | MiddleB 
                 | Other Int deriving (Ord,Eq,Show,Read)
button 1 = LeftB
button 2 = RightB
button 3 = MiddleB
button n = Other (fromIntegral n-4)
data MouseState = Pressed MouseButton | Released MouseButton | Moved deriving (Ord,Eq,Show,Read)

newWindow :: Integral a => String -> (a,a) -> IO Window
newWindow title (width, height) = do
  ctitle <- newCString title
  let (w,h) = (fromIntegral width, fromIntegral height)
  r <- new_window ctitle w h
  free ctitle
  op <- newIORef True
  ptr <- newForeignPtr close_window r
  return $ Window (ptr, op)

closeWindow :: Window -> IO ()
closeWindow (Window (p, r)) = do
  x <- readIORef r
  if not x
    then return ()
    else do
    writeIORef r False
    finalizeForeignPtr p
    writeIORef r False -- just in case!

withWindow :: Window -> (WindowP -> IO a) -> IO a
withWindow (Window (p,o)) f = do
  open <- readIORef o
  if open then withForeignPtr p f else error "Window already closed"

type Event = String

pollEvent :: Window -> IO Event
waitEvent :: Window -> IO Event
getEvent :: Bool -> Window -> IO Event
pollEvent = getEvent False
waitEvent = getEvent True
getEvent block win = do
  cstr <- withWindow win $ \w -> get_event_sexp w (if block then 1 else 0)
  str <- peekCString cstr
  free_event cstr
  return str
clearEvents win = pollEvent win 
  >>= \x -> if x == "NIL" || x == "CLOSED" then return () else clearEvents win

-- | Render some display list for a window
-- note that render may be called after the AWAKENED event has been recieved,
-- or when the window has just opened. Consider renderGetEvents instead.
render :: Window -> Builder a -> IO a
render win builder =
  withWindow win $ \win -> do
  (dlb,w,h) <- alloca $ \wp ->
    alloca $ \hp -> (\x y z -> (x,y,z)) <$>
                    display_list_builder win wp hp
                    <*> peek wp <*> peek hp
  a <- runBuilder builder (dlb,w,h)
  display_list_builder_build win dlb
  return a

-- | Render some display list and then collect all events until the
-- awakened event signifying that a new frame may be build
renderGetEvents :: Window -> Builder a -> IO (a, [Event])
renderGetEvents win builder = let 
  collectEvents = waitEvent win >>= collectEvent
  collectEvent "AWAKENED" = collectEvents'
  collectEvent "CLOSED" = return ["CLOSED"]
  collectEvent x          = (x:) <$> collectEvents
  collectEvents' = pollEvent win >>= collectEvent'
  collectEvent' "NIL"     = return []
  collectEvent' "CLOSED"= return ["CLOSED"]
  collectEvent' x         = (x:) <$> collectEvents'
  in do
  a <- render win builder
  evs <- collectEvents
  return (a, evs)

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
addRect r c = Builder $ \(x,_,_) -> colour c $ rect r $ display_list_builder_push_rect x

-- | Draw a border for a specified (rounded) rectangle with styles specified for each side.
addBorder :: Rect -> BorderRadius -> BorderSides -> Builder ()
addBorder r br bs = Builder $ \(x,_,_) ->
  radii br $ sides $ widths $ rect r $ display_list_builder_push_border_n x where
  widths f = 
    let BorderSides (BorderSide l _ _) (BorderSide t _ _) (BorderSide r _ _) (BorderSide b _ _) = bs in
      f l t r b
  sides f = side (bottom bs) $ side (right bs) $ side (top bs) $ side (left bs) $ f
  side (BorderSide _ c s) f = (colour c $ f) (BS.toCInt s)
  radii (BorderRadius (tlw,tlh) (trw,trh) (blw,blh) (brw,brh)) f = f tlw tlh trw trh blw blh brw brh


type FontFamily = String
type FontSize = Float
data Italicness = Upright | Italic deriving (Eq, Ord, Show, Read)
newtype Weight = Weight Word16 deriving (Eq, Ord)
normalWeight = Weight 400
bold = Weight 700
weight n = let n' = (n `div` 100) * 100 in
  Weight (max 100 (min 900 (fromIntegral n')))
instance Show Weight where
  showsPrec d w = case w of
    Weight 400 -> showString "normalWeight"
    Weight 700 -> showString "bold"
    Weight w   -> showParen (d > 10) $ showString "weight " . showsPrec 11 w
data Stretchiness = UltraCondensed | ExtraCondensed | Condensed | SemiCondensed | NormalStretch
                  | SemiExpanded | Expanded | ExtraExpanded | UltraExpanded
  deriving (Eq, Ord, Show, Read)
data FontFaceDescription = FontFace FontFamily Italicness Weight Stretchiness
  deriving (Eq, Ord, Show)

getFont :: FontFaceDescription -> FontSize -> Builder Font
getFont (FontFace fam ital wght strtch) size = let
  italic = if ital == Italic then 1 else 0
  Weight weight = wght
  stretch = case strtch of
    UltraCondensed -> 1; ExtraCondensed -> 2; Condensed -> 3; SemiCondensed -> 4
    NormalStretch -> 5; SemiExpanded -> 6; Expanded -> 7; ExtraExpanded -> 8
    UltraExpanded -> 9 in
  Builder $ \(d,_,_) -> do
  str <- newCString fam
  fp <- find_font d str size italic weight stretch 
  f <- newForeignPtr free_font fp
  free str
  return f

-- | Draw some text inside a rectangle. The text is given the (local)
-- position of the first character (with the y-coordinate being the baseline).
addText' :: Rect -> (Float, Float) -> Font -> Colour -> String -> Builder ()
addText' r@(Rect x y _ _) (ox, oy) font c text = Builder $ \(d,_,_) -> do
  str <- newCString text
  withForeignPtr font $ \font -> do
    (colour c $ (rect r $ display_list_builder_push_text d) font) (x+ox) (y+oy) str
  free str

shapeWidthsFree :: Ptr ShapedRunInfo -> CSize -> IO ()
shapeWidthsFree p len = shaped_text_widths_free p len
data ShapedText = ShapedText (ForeignPtr ShapedText_s) (V.Vector ShapedRunInfo) deriving (Show)
shapeText :: Font -> String -> IO ShapedText
shapeText font text = do
  str <- newCString text
  st <- withForeignPtr font (\f -> shape_text f str)
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
shapeText' :: Font -> String -> Builder ShapedText
shapeText' font text = Builder $ const $ shapeText font text

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
addText r c (LaidOutText s v) = Builder $ \(d,_,_) -> 
  withForeignPtr s $ \s ->
  V.unsafeWith v $ \v' -> do
  (colour c $ rect r $ display_list_builder_push_shaped_text d) s v' (fromIntegral $ V.length v)

-- | Some simple colours
black, red, green, blue, white :: Colour
black = Colour 0 0 0 1
red =   Colour 1 0 0 1
green = Colour 0 1 0 1
blue =  Colour 0 0 1 1
white = Colour 1 1 1 1

