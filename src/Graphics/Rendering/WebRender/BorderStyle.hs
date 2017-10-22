module Graphics.Rendering.WebRender.BorderStyle
  ( BorderStyle(..)
  , toCInt 
  ) where

import Foreign.C.Types (CInt(..))

-- | A simple enum for all the different types of border
-- supported by WebRender.
data BorderStyle = None -- ^ Invisible border
                 | Solid -- ^ A solid line
                 | Double -- ^ Two parallel lines (each line 1/3 of the border width)
                 | Dotted -- ^ Circular dots (the diameter is the border width and the circles are separated by diameters)
                 | Dashed -- ^ Rectangular dashes
                 | Hidden -- ^ The same as None
                 | Groove -- ^ Outset inside Inset
                 | Ridge -- ^ Inset inside Outset
                 | Inset -- ^ A darker colour on the top and left and a lighter colour on the bottom and right
                 | Outset -- ^ A lighter colour on the top and left and a darker colour on the bottom and right
                 deriving (Eq, Enum, Show, Read)

-- | Convert a BorderStyle to the CInt used in the api to represent it
toCInt :: BorderStyle -> CInt
toCInt s = case s of
  None -> 0; Solid -> 1; Double -> 2; Dotted -> 3; Dashed -> 4; Hidden -> 5; Groove -> 6;
  Ridge -> 7; Inset -> 8; Outset -> 9

