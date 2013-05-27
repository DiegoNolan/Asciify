
module Asciify
 ( quadAsciify
 , scaleAsciify
 , test
 ) where

import Codec.Picture
import Codec.Picture.Types
import Data.Word
import Data.Either.Unwrap
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V
import Data.Char
import System.IO

-- Contains the averages of the grayscale pixel values in each quadrant
type CharShape = (Word8, Word8, Word8, Word8)

-- Ratinal coordinate
type RCord = (Rational, Rational)

-- A Quadrant of a character, only has three stages now
data Quadrant = Blank | Light | Dark deriving (Show, Ord, Eq)

-- This is the ratio we use to adjust for the fact that characters are not squares.
widthToHeight:: Rational
widthToHeight = (2/3)

testGSImage fname = do
   dy <- readImage fname
   if notSupportedYet (fromRight dy) then
      putStrLn "Not supported"
   else
      putStrLn "supported"
   return $ grayscaleImage (fromRight dy)

-- String helper, should be moved
remFileExt :: String -> String
remFileExt = reverse . (drop 1) . ( dropWhile (/='.')) . reverse

-- Run both examples
test fname w = do
   img <- testGSImage fname
   handle <- openFile ((remFileExt fname) ++ "_quad.txt") WriteMode
   let str = quadAsciify img w
   putStr str
   hPutStr handle str

   h2 <- openFile ((remFileExt fname) ++ "_scale.txt") WriteMode
   let str = scaleAsciify img w
   putStr str
   hPutStr h2 str

-- Takes grayslake Image and returns a string of ascii characters
-- representing the image that is the given width wide in characters
-- Splits each character into a quadrant
quadAsciify :: Image Pixel8 -> Int -> String
quadAsciify iMG chsWide = foldl (\acc j -> acc ++ row j ++ "\n") ""
   [0..(chsHigh)]
   where img = normPixels iMG
         dim = charDims img chsWide
         chsHigh = ceiling ((fromIntegral (imageHeight img))/(snd dim))
         row c = map (\i -> shapeToAscii (shape img
               ((fromIntegral i)*(fst dim),(fromIntegral c)*(snd dim)) dim) )
               [0..(chsWide-1)]

-- Same thing as above but uses a different algorithm
-- Each characte is considered in its enitirity but has more regions
scaleAsciify :: Image Pixel8 -> Int -> String
scaleAsciify iMG chsWide = foldl (\acc j -> acc ++ row j ++ "\n") ""
   [0..(chsHigh)]
   where img = normPixels iMG
         dim = charDims img chsWide
         chsHigh = ceiling ((fromIntegral (imageHeight img))/(snd dim))
         row c = map (\i -> asciiReplace $ meanInRect img
               ((fromIntegral i)*(fst dim),(fromIntegral c)*(snd dim)) dim )
               [0..(chsWide-1)]

-- Normalize the grayscale image so the darkest pixel is zero and the
-- brightest is 255
normPixels :: Image Pixel8 -> Image Pixel8
normPixels img = pixelMap
   (\p -> floor (((fromIntegral p)-pMin)*(255/(pMax-pMin)))) img
   where pMax = fromIntegral (V.foldl' (\acc i -> max acc i) 0 dat)::Rational
         pMin = fromIntegral (V.foldl' (\acc i -> min acc i) 255 dat)::Rational
         dat  = imageData img

weigthedMean :: [(Word8,Rational)] -> Rational
weigthedMean xs = if den /= 0 then num / den else 255 -- Catch divide by zeros
   where num = foldl (\acc (a,b) -> (fromIntegral a)*b + acc) 0 xs
         den = foldl (\acc (_,b) -> acc + b) 0 xs

shape :: (Image Pixel8) -> --grayscale image
   RCord -> -- Offset from the upper left of the image
   RCord -> -- dimensions of the rectangle
   CharShape -- The averages of each quadrant
shape img os dim = ( (m os hd) , (m (hx,snd os) hd) ,
   (m (fst os, hy) hd) , (m (hx,hy) hd) )
      where m = meanInRect img
            hd = ((fst dim)/2, (snd dim)/2)
            hx = (fst os) + (fst hd)
            hy = (snd os) + (snd hd)

-- Returns the dimensions of a character in pixels
charDims :: (Image Pixel8) ->
   Int -> --How many characters wide the ascii plans to be
   RCord -- Width x Height
charDims img charsWide = (xdim, xdim/widthToHeight)
   where w = (fromIntegral . imageWidth) img :: Rational
         h = (fromIntegral . imageHeight) img :: Rational
         xdim = w / (fromIntegral charsWide)

-- Returns the mean weighted pixel value in a rectangle of a grayscale image
meanInRect :: (Image Pixel8) -> RCord -> RCord -> Word8
meanInRect img (x,y) (xd,yd) = floor $ weigthedMean valAndWgt
   where maxX = (imageWidth img) - 1
         maxY = (imageHeight img) - 1
         inds = [ (i,j) | i <- xinds, j <- yinds]
         xinds = [(floor x)..(min (floor (x+xd)) maxX) ]
         yinds = [(floor y)..(min (floor (y+yd)) maxY) ]
         area xi yi = ((x2 xi)-(x1 xi))*((y2 xi)-(y1 xi))
         x1 xi = max (fromIntegral xi) x
         x2 xi = min (fromIntegral (xi+1)) (x+xd)
         y1 yi = max (fromIntegral yi) y
         y2 yi = min (fromIntegral (yi+1)) (y+yd)
         valAndWgt = map (\(a,b) -> (pixelAt img a b,area a b)) inds

-- Convert a dynamic image to grayscale of its image
grayscaleImage :: DynamicImage -> Image Pixel8
grayscaleImage dyImage = case dyImage of
   ImageY8     pix8       -> pix8
   ImageYA8    pixYA8     -> pixelMap computeLuma pixYA8 -- untested
   ImageRGB8   pixRGB8    -> pixelMap computeLuma pixRGB8 -- works
   ImageRGBA8  pixRGBA8   -> pixelMap computeLuma pixRGBA8
   ImageYCbCr8 pixYCbCr8  -> pixelMap computeLuma pixYCbCr8 -- untested

dyImageType :: DynamicImage -> String
dyImageType dyImage = case dyImage of
   ImageY8     pix8       -> "ImageY8"
   ImageYA8    pixYA8     -> "ImageYA8"
   ImageRGB8   pixRGB8    -> "ImageRGB8"
   ImageRGBA8  pixRGBA8   -> "ImageRGBA8"
   ImageYCbCr8 pixYCbCr8  -> "ImageYCbCr8"

notSupportedYet :: DynamicImage -> Bool
notSupportedYet dyImage = case dyImage of
   ImageY8     pix8       -> False
   ImageYA8    pixYA8     -> True
   ImageRGB8   pixRGB8    -> False
   ImageRGBA8  pixRGBA8   -> True
   ImageYCbCr8 pixYCbCr8  -> True

shapeToAscii :: CharShape -> Char
shapeToAscii = aSToA . aShape

aShape :: CharShape -> (Quadrant,Quadrant,Quadrant,Quadrant)
aShape (a,b,c,d) = (approx a,approx b,approx c,approx d)

approx :: Word8 -> Quadrant
approx n
   | n >= 150  = Blank
   | n >= 70  = Light
   | otherwise = Dark

-- Character Mappings
-- Simple grayscale value to character
asciiReplace :: Word8 -> Char
asciiReplace n
   | n >= 240      = ' '
   | n >= 220      = '`'
   | n >= 200      = ','
   | n >= 180      = '-'
   | n >= 160      = '~'
   | n >= 140      = '+'
   | n >= 120      = '='
   | n >= 100      = 'i'
   | n >= 80       = 'T'
   | n >= 60       = 'O'
   | n >= 40       = '0'
   | n >= 20       = 'Z'
   | otherwise     = 'M'

-- Approximate shape to character
aSToA :: (Quadrant,Quadrant,Quadrant,Quadrant) -> Char
aSToA (Blank, Blank, Blank, Blank) = ' '
aSToA (Blank, Blank, Blank, Light) = '.'
aSToA (Blank, Blank, Blank, Dark)  = '.'
aSToA (Blank, Blank, Light, Blank) = ','
aSToA (Blank, Blank, Light, Light) = '_'
aSToA (Blank, Blank, Light, Dark)  = '_'
aSToA (Blank, Blank, Dark, Blank)  = '_'
aSToA (Blank, Blank, Dark, Light)  = '_'
aSToA (Blank, Blank, Dark, Dark)   = 'd'
aSToA (Blank, Light, Blank, Blank) = 'J'
aSToA (Blank, Light, Blank, Light) = 'j'
aSToA (Blank, Light, Blank, Dark)  = ','
aSToA (Blank, Light, Light, Blank) = 'j'
aSToA (Blank, Light, Light, Light) = 'j'
aSToA (Blank, Light, Light, Dark)  = 'j'
aSToA (Blank, Light, Dark, Blank)  = '/'
aSToA (Blank, Light, Dark, Light)  = '/'
aSToA (Blank, Light, Dark, Dark)   = '_'
aSToA (Blank, Dark, Blank, Blank)  = ','
aSToA (Blank, Dark, Blank, Light)  = '_'
aSToA (Blank, Dark, Blank, Dark)   = ']'
aSToA (Blank, Dark, Light, Blank)  = '/'
aSToA (Blank, Dark, Light, Light)  = '\''
aSToA (Blank, Dark, Light, Dark)   = 'J'
aSToA (Blank, Dark, Dark, Blank)   = '_'
aSToA (Blank, Dark, Dark, Light)   = ' '
aSToA (Blank, Dark, Dark, Dark)    = 'J'
aSToA (Light, Blank, Blank, Blank) = '\''
aSToA (Light, Blank, Blank, Light) = '\\'
aSToA (Light, Blank, Blank, Dark)  = '\\'
aSToA (Light, Blank, Light, Blank) = '('
aSToA (Light, Blank, Light, Light) = 'h'
aSToA (Light, Blank, Light, Dark)  = 'k'
aSToA (Light, Blank, Dark, Blank)  = '<'
aSToA (Light, Blank, Dark, Light)  = 'k'
aSToA (Light, Blank, Dark, Dark)   = '_'
aSToA (Light, Light, Blank, Blank) = '\''
aSToA (Light, Light, Blank, Light) = '7'
aSToA (Light, Light, Blank, Dark)  = '7'
aSToA (Light, Light, Light, Blank) = 'T'
aSToA (Light, Light, Light, Light) = 'O'
aSToA (Light, Light, Light, Dark)  = 'Q'
aSToA (Light, Light, Dark, Blank)  = 'r'
aSToA (Light, Light, Dark, Light)  = '\\'
aSToA (Light, Light, Dark, Dark)   = 'j'
aSToA (Light, Dark, Blank, Blank)  = '`'
aSToA (Light, Dark, Blank, Light)  = '7'
aSToA (Light, Dark, Blank, Dark)   = '7'
aSToA (Light, Dark, Light, Blank)  = 'P'
aSToA (Light, Dark, Light, Light)  = '9'
aSToA (Light, Dark, Light, Dark)   = ']'
aSToA (Light, Dark, Dark, Blank)   = '/'
aSToA (Light, Dark, Dark, Light)   = '?'
aSToA (Light, Dark, Dark, Dark)    = 'd'
aSToA (Dark, Blank, Blank, Blank)  = '`'
aSToA (Dark, Blank, Blank, Light)  = '\\'
aSToA (Dark, Blank, Blank, Dark)   = '\\'
aSToA (Dark, Blank, Light, Blank)  = 'b'
aSToA (Dark, Blank, Light, Light)  = 'L'
aSToA (Dark, Blank, Light, Dark)   = '\\'
aSToA (Dark, Blank, Dark, Blank)   = '('
aSToA (Dark, Blank, Dark, Light)   = 'L'
aSToA (Dark, Blank, Dark, Dark)    = 'L'
aSToA (Dark, Light, Blank, Blank)  = 'L'
aSToA (Dark, Light, Blank, Light)  = '7'
aSToA (Dark, Light, Blank, Dark)   = '9'
aSToA (Dark, Light, Light, Blank)  = 'T'
aSToA (Dark, Light, Light, Light)  = 'F'
aSToA (Dark, Light, Light, Dark)   = 'E'
aSToA (Dark, Light, Dark, Blank)   = 'F'
aSToA (Dark, Light, Dark, Light)   = '%'
aSToA (Dark, Light, Dark, Dark)    = 'k'
aSToA (Dark, Dark, Blank, Blank)   = '"'
aSToA (Dark, Dark, Blank, Light)   = '*'
aSToA (Dark, Dark, Blank, Dark)    = '9'
aSToA (Dark, Dark, Light, Blank)   = '"'
aSToA (Dark, Dark, Light, Light)   = 'Y'
aSToA (Dark, Dark, Light, Dark)    = '9'
aSToA (Dark, Dark, Dark, Blank)    = 'F'
aSToA (Dark, Dark, Dark, Light)    = '#'
aSToA (Dark, Dark, Dark, Dark)     = '@'
