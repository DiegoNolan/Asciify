
module Asciify
 ( asciify
 , Algorithm
 , novemAsciify
 , quadAsciify
 , scaleAsciify
 , testAllAlgorithms
 ) where

import Data.List
import Data.Word
import Data.Char
import System.IO
import qualified Data.Vector.Storable as V
import Codec.Picture
import Codec.Picture.Types

data Algorithm = Single | Quad | Novem

-- Contains the averages of the grayscale pixel values in each quadrant
type CharShape = (Word8, Word8, Word8, Word8)

data NovemShape = NovemShape !Word8 !Word8 !Word8
                             !Word8 !Word8 !Word8
                             !Word8 !Word8 !Word8

-- Ratinal coordinate
type RCord = (Rational, Rational)

-- A Quadrant of a character, only has three stages now
data Quadrant = Blank | Light | Dark deriving (Show, Ord, Eq)

-- For Novemant method
data Brightness = Bnk | Lgt | Drk | Bck deriving (Show, Ord, Eq)

data Novemant = Novemant !Brightness !Brightness !Brightness
                         !Brightness !Brightness !Brightness
                         !Brightness !Brightness !Brightness
                         deriving (Show, Ord, Eq)

-- This is the ratio we use to adjust for the fact that characters are not squares.
widthToHeight:: Rational
widthToHeight = (2/3)

loadGSImage :: String -> IO (Maybe (Image Pixel8) )
loadGSImage fname = do
   dy <- readImage fname
   case dy of
      Right img -> return (Just $ grayscaleImage img)
      Left str  -> do
         putStrLn $ str
         return Nothing

-- String helper, should be moved
remFileExt :: String -> String
remFileExt = reverse . (drop 1) . ( dropWhile (/='.')) . reverse

-- Run All examples in terminal
testAllAlgorithms :: String -> Int -> IO ()
testAllAlgorithms fname w = do
   img' <- loadGSImage fname
   case img' of
      Just image -> do
         let img = normPixels image

         h1 <- openFile ((remFileExt fname) ++ "_novem.txt") WriteMode
         let str = novemAsciify img w
         putStr str
         hPutStr h1 str

         h2 <- openFile ((remFileExt fname) ++ "_quad.txt") WriteMode
         let str = quadAsciify img w
         putStr str
         hPutStr h2 str

         h3 <- openFile ((remFileExt fname) ++ "_scale.txt") WriteMode
         let str = scaleAsciify img w
         putStr str
         hPutStr h3 str
      Nothing     -> return ()

asciify :: String -> Int -> Algorithm -> IO (Maybe String)
asciify fileName charWidth alg = do
   mImg <- loadGSImage fileName
   case mImg of
      Just img -> return (Just ((getAlgorithm alg) img charWidth) )
      Nothing  -> return Nothing

getAlgorithm :: Algorithm -> ( Image Pixel8 -> Int -> String )
getAlgorithm Single  = scaleAsciify
getAlgorithm Quad    = quadAsciify
getAlgorithm Novem   = novemAsciify

novemAsciify :: Image Pixel8 -> Int -> String
novemAsciify img chsWide = foldl (\acc j -> acc ++ row j ++ "\n") ""
   [0..chsHigh]
   where dim = charDims img chsWide
         chsHigh = floor -- used to be ceiling, unsure which one it should be
            ((fromIntegral (imageHeight img))/(snd dim))
         row c = map (\i -> nShapeToAsccii (nShape img
               ((fromIntegral i)*(fst dim),(fromIntegral c)*(snd dim)) dim) )
               [0..(chsWide-1)]

-- Takes grayslake Image and returns a string of ascii characters
-- representing the image that is the given width wide in characters
-- Splits each character into a quadrant
quadAsciify :: Image Pixel8 -> Int -> String
quadAsciify img chsWide = foldl (\acc j -> acc ++ row j ++ "\n") ""
   [0..(chsHigh)]
   where dim = charDims img chsWide
         chsHigh = floor -- used to be ceiling, unsure which one it should be
            ((fromIntegral (imageHeight img))/(snd dim))
         row c = map (\i -> shapeToAscii (shape img
               ((fromIntegral i)*(fst dim),(fromIntegral c)*(snd dim)) dim) )
               [0..(chsWide-1)]

-- Same thing as above but uses a different algorithm
-- Each characte is considered in its enitirity but has more regions
scaleAsciify :: Image Pixel8 -> Int -> String
scaleAsciify img chsWide = foldl (\acc j -> acc ++ row j ++ "\n") ""
   [0..(chsHigh)]
   where dim = charDims img chsWide
         chsHigh = floor ((fromIntegral (imageHeight img))/(snd dim))
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

nShape :: (Image Pixel8) ->
   RCord ->
   RCord ->
   NovemShape
nShape img (ox,oy) dim = NovemShape
   (m (ox,oy) td)       (m (ox+tdx,oy) td)       (m (ox+2*tdx,oy) td)
   (m (ox,oy+tdy) td)   (m (ox+tdx,oy+tdy) td)   (m (ox+2*tdx,oy+tdy) td)
   (m (ox,oy+2*tdy) td) (m (ox+tdx,oy+2*tdy) td) (m (ox+2*tdx,oy+2*tdy) td)
   where m = meanInRect img
         td = ((fst dim)/3, (snd dim)/3)
         (tdx,tdy) = td

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
   ImageYA8    pixYA8     -> pixelMap computeLuma pixYA8
   ImageRGB8   pixRGB8    -> pixelMap computeLuma pixRGB8
   ImageRGBA8  pixRGBA8   -> pixelMap computeLuma pixRGBA8
   ImageYCbCr8 pixYCbCr8  -> pixelMap computeLuma pixYCbCr8

dyImageType :: DynamicImage -> String
dyImageType dyImage = case dyImage of
   ImageY8     pix8       -> "ImageY8"
   ImageYA8    pixYA8     -> "ImageYA8"
   ImageRGB8   pixRGB8    -> "ImageRGB8"
   ImageRGBA8  pixRGBA8   -> "ImageRGBA8"
   ImageYCbCr8 pixYCbCr8  -> "ImageYCbCr8"

shapeToAscii :: CharShape -> Char
shapeToAscii = aSToA . aShape

aShape :: CharShape -> (Quadrant,Quadrant,Quadrant,Quadrant)
aShape (a,b,c,d) = (approx a,approx b,approx c,approx d)

nShapeToAsccii :: NovemShape -> Char
nShapeToAsccii = (bestMatch allChars) . novShape

allChars = map chr [32..126]

novShape :: NovemShape -> Novemant
novShape (NovemShape a b c d e f g h i) =
   Novemant (novemApprox a) (novemApprox b) (novemApprox c)
            (novemApprox d) (novemApprox e) (novemApprox f)
            (novemApprox g) (novemApprox h) (novemApprox i)

approx :: Word8 -> Quadrant
approx n
   | n >= 150  = Blank
   | n >= 70  = Light
   | otherwise = Dark

novemApprox :: Word8 -> Brightness
novemApprox n
   | n >= 150  = Bnk
   | n >= 90   = Lgt
   | n >= 25   = Drk
   | otherwise = Bck

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

bestMatch :: String -> Novemant -> Char
bestMatch chs n = snd $! foldl' (\(mean,char) (mnext, chnext) -> if mnext < mean then
      (mnext,chnext) else (mean,char) ) (100000, ' ') meanAndnov
   where meanAndnov = map (\x -> (meanSquare n (charToNovem x), x)) chs

-- Gets the mean square value of the one movemant compared to another
-- There has to be a better way to write this function
meanSquare :: Novemant -> Novemant -> Float
meanSquare (Novemant a b c d e f g h i) (Novemant j k l m n o p q r)
   = sqrt $!
      (brightDiff a j)**2 +
      (brightDiff b k)**2 +
      (brightDiff c l)**2 +
      (brightDiff d m)**2 +
      (brightDiff e n)**2 +
      (brightDiff f o)**2 +
      (brightDiff g p)**2 +
      (brightDiff h q)**2 +
      (brightDiff i r)**2

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

brightDiff :: Brightness -> Brightness -> Float
brightDiff Bnk Bnk = 0
brightDiff Bnk Lgt = 1
brightDiff Bnk Drk = 2
brightDiff Bnk Bck = 3
brightDiff Lgt Lgt = 0
brightDiff Lgt Drk = 1
brightDiff Lgt Bck = 2
brightDiff Drk Drk = 0
brightDiff Drk Bck = 1
brightDiff Bck Bck = 0
brightDiff x y = brightDiff y x

charToNovem :: Char -> Novemant
charToNovem ' '   = Novemant Bnk Bnk Bnk Bnk Bnk Bnk Bnk Bnk Bnk
charToNovem '!'   = Novemant Bnk Drk Bnk Bnk Drk Bnk Bnk Lgt Bnk
charToNovem '\"'  = Novemant Bnk Drk Bnk Bnk Bnk Bnk Bnk Bnk Bnk
charToNovem '#'   = Novemant Bnk Drk Drk Drk Bck Drk Drk Drk Bnk
charToNovem '$'   = Novemant Bnk Drk Bnk Drk Bck Drk Bnk Drk Bnk
charToNovem '%'   = Novemant Bck Lgt Bnk Drk Bck Drk Bnk Lgt Bck
charToNovem '&'   = Novemant Bnk Drk Lgt Drk Bck Lgt Drk Drk Drk
charToNovem '\''  = Novemant Bnk Lgt Bnk Bnk Bnk Bnk Bnk Bnk Bnk
charToNovem '('   = Novemant Bnk Drk Bnk Drk Lgt Bnk Bnk Drk Bnk
charToNovem ')'   = Novemant Bnk Drk Bnk Bnk Lgt Drk Bnk Drk Bnk
charToNovem '*'   = Novemant Lgt Bck Lgt Bnk Lgt Bnk Bnk Bnk Bnk
charToNovem '+'   = Novemant Bnk Drk Bnk Drk Bck Drk Bnk Drk Bnk
charToNovem ','   = Novemant Bnk Bnk Bnk Bnk Bnk Bnk Bnk Drk Bnk
charToNovem '-'   = Novemant Bnk Bnk Bnk Lgt Drk Lgt Bnk Bnk Bnk
charToNovem '.'   = Novemant Bnk Bnk Bnk Bnk Bnk Bnk Bnk Lgt Bnk
charToNovem '/'   = Novemant Bnk Bnk Drk Bnk Drk Bnk Drk Bnk Bnk
charToNovem '0'   = Novemant Lgt Drk Lgt Drk Lgt Drk Lgt Drk Lgt
charToNovem '1'   = Novemant Bnk Drk Bnk Bnk Lgt Bnk Lgt Drk Lgt
charToNovem '2'   = Novemant Lgt Drk Lgt Bnk Lgt Drk Lgt Drk Drk
charToNovem '3'   = Novemant Lgt Drk Drk Bnk Drk Drk Lgt Bnk Bnk
charToNovem '4'   = Novemant Bnk Drk Drk Drk Drk Drk Bnk Bnk Lgt
charToNovem '5'   = Novemant Drk Drk Drk Drk Lgt Bnk Bnk Drk Lgt
charToNovem '6'   = Novemant Bnk Drk Lgt Drk Drk Lgt Lgt Drk Lgt
charToNovem '7'   = Novemant Lgt Drk Drk Bnk Drk Lgt Drk Bnk Bnk
charToNovem '8'   = Novemant Lgt Drk Lgt Lgt Drk Lgt Lgt Drk Lgt
charToNovem '9'   = Novemant Lgt Lgt Lgt Lgt Drk Lgt Lgt Bck Lgt
charToNovem ':'   = Novemant Bnk Lgt Bnk Bnk Lgt Bnk Bnk Lgt Bnk
charToNovem ';'   = Novemant Bnk Lgt Bnk Bnk Lgt Bnk Bnk Drk Bnk
charToNovem '<'   = Novemant Bnk Lgt Lgt Lgt Drk Lgt Bnk Lgt Lgt
charToNovem '='   = Novemant Bnk Bnk Bnk Bck Bck Bck Bnk Bnk Bnk
charToNovem '>'   = Novemant Lgt Lgt Bnk Lgt Drk Lgt Lgt Lgt Bnk
charToNovem '?'   = Novemant Lgt Drk Lgt Bnk Drk Lgt Bnk Bnk Drk
charToNovem '@'   = Novemant Lgt Drk Lgt Drk Bck Drk Lgt Drk Lgt
charToNovem 'A'   = Novemant Bnk Drk Bnk Lgt Bck Lgt Drk Bnk Drk
charToNovem 'B'   = Novemant Drk Drk Lgt Bck Bck Drk Drk Drk Lgt
charToNovem 'C'   = Novemant Lgt Drk Lgt Drk Bnk Bnk Lgt Drk Lgt
charToNovem 'D'   = Novemant Drk Lgt Bnk Drk Bnk Drk Drk Lgt Bnk
charToNovem 'E'   = Novemant Bck Drk Drk Bck Drk Drk Bck Drk Drk
charToNovem 'F'   = Novemant Bck Drk Drk Bck Drk Drk Bck Bnk Bnk
charToNovem 'G'   = Novemant Lgt Drk Lgt Drk Drk Drk Lgt Drk Lgt
charToNovem 'H'   = Novemant Lgt Bnk Lgt Drk Drk Drk Lgt Bnk Lgt
charToNovem 'I'   = Novemant Lgt Drk Lgt Bnk Drk Bnk Lgt Drk Lgt
charToNovem 'J'   = Novemant Bnk Lgt Drk Bnk Bnk Drk Lgt Drk Lgt
charToNovem 'K'   = Novemant Lgt Lgt Lgt Drk Bck Bnk Lgt Lgt Lgt
charToNovem 'L'   = Novemant Lgt Bnk Bnk Lgt Bnk Bnk Drk Lgt Lgt
charToNovem 'M'   = Novemant Drk Drk Drk Bck Bck Bck Drk Bnk Drk
charToNovem 'N'   = Novemant Bck Lgt Drk Drk Bck Drk Drk Lgt Bck
charToNovem 'O'   = Novemant Lgt Drk Lgt Drk Bnk Drk Lgt Drk Lgt
charToNovem 'P'   = Novemant Drk Lgt Lgt Drk Drk Lgt Lgt Bnk Bnk
charToNovem 'Q'   = Novemant Lgt Drk Lgt Drk Bnk Drk Lgt Drk Drk
charToNovem 'R'   = Novemant Bck Drk Lgt Bck Bck Drk Drk Bnk Lgt
charToNovem 'S'   = Novemant Lgt Drk Lgt Drk Drk Drk Lgt Drk Lgt
charToNovem 'T'   = Novemant Lgt Drk Lgt Bnk Drk Bnk Bnk Lgt Bnk
charToNovem 'U'   = Novemant Drk Bnk Drk Drk Bnk Drk Lgt Drk Lgt
charToNovem 'V'   = Novemant Drk Bnk Drk Bnk Drk Bnk Bnk Bck Bnk
charToNovem 'W'   = Novemant Drk Bnk Drk Lgt Bck Lgt Bnk Bck Bnk
charToNovem 'X'   = Novemant Drk Bnk Drk Bnk Bck Bnk Drk Bnk Drk
charToNovem 'Y'   = Novemant Lgt Bnk Lgt Bnk Drk Bnk Bnk Drk Bnk
charToNovem 'Z'   = Novemant Lgt Drk Drk Bnk Drk Bnk Drk Drk Lgt
charToNovem '['   = Novemant Lgt Lgt Bnk Drk Bnk Bnk Lgt Lgt Bnk
charToNovem '\\'  = Novemant Drk Bnk Bnk Bnk Drk Bnk Bnk Bnk Drk
charToNovem ']'   = Novemant Lgt Lgt Bnk Bnk Bnk Drk Lgt Lgt Bnk
charToNovem '^'   = Novemant Lgt Drk Lgt Bnk Bnk Drk Drk Drk Drk
charToNovem '_'   = Novemant Bnk Bnk Bnk Bnk Bnk Bnk Lgt Drk Lgt
charToNovem '`'   = Novemant Lgt Drk Bnk Bnk Bnk Bnk Bnk Bnk Bnk
charToNovem 'a'   = Novemant Bnk Bnk Bnk Drk Bck Drk Drk Drk Drk
charToNovem 'b'   = Novemant Lgt Bnk Bnk Drk Drk Drk Drk Drk Drk
charToNovem 'c'   = Novemant Bnk Bnk Bnk Lgt Drk Lgt Lgt Drk Lgt
charToNovem 'd'   = Novemant Bnk Bnk Lgt Drk Drk Drk Drk Drk Drk
charToNovem 'e'   = Novemant Bnk Bnk Bnk Drk Drk Drk Drk Drk Drk
charToNovem 'f'   = Novemant Bnk Lgt Lgt Lgt Lgt Lgt Bnk Lgt Bnk
charToNovem 'g'   = Novemant Bnk Bnk Bnk Drk Drk Drk Drk Drk Drk
charToNovem 'h'   = Novemant Lgt Bnk Bnk Drk Drk Drk Lgt Bnk Lgt
charToNovem 'i'   = Novemant Bnk Lgt Bnk Bnk Lgt Bnk Lgt Drk Lgt
charToNovem 'j'   = Novemant Bnk Lgt Bnk Bnk Drk Bnk Lgt Drk Lgt
charToNovem 'k'   = Novemant Lgt Bnk Bnk Drk Drk Drk Lgt Bnk Lgt
charToNovem 'l'   = Novemant Lgt Drk Bnk Bnk Drk Bnk Bnk Lgt Lgt
charToNovem 'm'   = Novemant Bnk Bnk Bnk Drk Bck Drk Drk Drk Drk
charToNovem 'n'   = Novemant Bnk Bnk Bnk Drk Drk Drk Lgt Bnk Lgt
charToNovem 'o'   = Novemant Bnk Bnk Bnk Lgt Drk Lgt Lgt Drk Lgt
charToNovem 'p'   = Novemant Bnk Bnk Bnk Drk Drk Bck Drk Drk Lgt
charToNovem 'q'   = Novemant Bnk Bnk Bnk Drk Drk Bck Lgt Drk Drk
charToNovem 'r'   = Novemant Bnk Bnk Bnk Lgt Drk Lgt Lgt Bnk Bnk
charToNovem 's'   = Novemant Bnk Bnk Bnk Lgt Drk Lgt Lgt Drk Lgt
charToNovem 't'   = Novemant Bnk Lgt Bnk Lgt Drk Lgt Bnk Drk Lgt
charToNovem 'u'   = Novemant Bnk Bnk Bnk Lgt Bnk Lgt Lgt Drk Drk
charToNovem 'v'   = Novemant Bnk Bnk Bnk Lgt Bnk Lgt Bnk Lgt Bnk
charToNovem 'w'   = Novemant Bnk Bnk Bnk Lgt Lgt Lgt Lgt Drk Lgt
charToNovem 'x'   = Novemant Bnk Bnk Bnk Drk Drk Drk Drk Drk Drk
charToNovem 'y'   = Novemant Bnk Bnk Bnk Drk Drk Drk Drk Drk Drk
charToNovem 'z'   = Novemant Bnk Bnk Bnk Drk Drk Drk Drk Drk Drk
charToNovem '{'   = Novemant Bnk Lgt Lgt Lgt Drk Bnk Bnk Lgt Lgt
charToNovem '|'   = Novemant Bnk Drk Bnk Bnk Drk Bnk Bnk Drk Bnk
charToNovem '}'   = Novemant Lgt Lgt Drk Bnk Drk Lgt Lgt Lgt Bnk
charToNovem '~'   = Novemant Bnk Bnk Bnk Drk Drk Drk Bnk Bnk Bnk

