import Codec.Picture
import Codec.Picture.Types
import Data.Word
import Data.Either.Unwrap
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V
import System.IO

type CharShape = (Word8, Word8, Word8, Word8)

type RCord = (Rational, Rational)

data Quadrant = Blank | Light | Dark

tl (x,_,_,_) = x
tr (_,x,_,_) = x
bl (_,_,x,_) = x
br (_,_,_,x) = x

testGSImage fname = do
   dy <- readImage fname
   return $ grayscaleImage (fromRight dy)

heightToWidth :: Rational
heightToWidth = (2/3)

test fname w = do
   img <- testGSImage fname
   handle <- openFile "test.txt" WriteMode
   let str = compAsciify img w
   putStr str
   hPutStr handle str

--compAsciify :: Image Pixel8 -> Int -> String
compAsciify img chsWide = foldl (\acc j -> acc ++ row j ++ "\n") "" [0..(chsHigh)]
   where dim = charDims img chsWide
         chsHigh = ceiling ((fromIntegral (imageHeight img))/(snd dim))
         row c = map (\i -> shapeToAscii (shape img
               ((fromIntegral i)*(fst dim),(fromIntegral c)*(snd dim)) dim) )
               [0..(chsWide-1)]


mean :: Fractional a => [a] -> a
mean [] = 0
mean xs = (sum xs) / (fromIntegral $ length xs)

weigthedMean :: [(Word8,Rational)] -> Rational
weigthedMean xs = if den /= 0 then num / den else 0
   where num = foldl (\acc (a,b) -> (fromIntegral a)*b + acc) 0 xs
         den = foldl (\acc (_,b) -> acc + b) 0 xs

shape :: (Image Pixel8) -> RCord -> RCord -> CharShape
shape img os dim = ( (m os hd) , (m (hx,snd os) hd) ,
   (m (fst os, hy) hd) , (m (hx,hy) hd) )
      where m = meanInRect img
            hd = ((fst dim)/2, (snd dim)/2)
            hx = (fst os) + (fst hd)
            hy = (snd os) + (snd hd)

-- Returns the dimensions of a character in pixels
charDims :: (Image Pixel8) -> Int -> RCord
charDims img charsWide = (xdim, xdim/heightToWidth)
   where w = (fromIntegral . imageWidth) img :: Rational
         h = (fromIntegral . imageHeight) img :: Rational
         xdim = w / (fromIntegral charsWide)

-- The dimensios should be half of the actual character dimension
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

asciify :: DynamicImage -> String
asciify = grayScaleToAscii . grayscaleImage

unsafe = do
   dyImg <- readImage "test.png"
   return $ (\(ImageY8 t) -> t) (fromRight dyImg)

invert :: (Image Pixel8) -> (Image Pixel8)
invert = pixelMap (\p -> (255 - p) :: Word8)

asciifyFile :: String -> String -> IO ( Maybe String )
asciifyFile inpath outpath = do
   dyImg <- readImage inpath
   if isRight dyImg then do
      handle <- openFile outpath WriteMode
      hPutStr handle $ (asciify . fromRight) dyImg
      return Nothing
   else return $ Just (fromLeft dyImg)

grayScaleToAscii :: (Image Pixel8) -> String
grayScaleToAscii img = gRow 0
   where v = imageData img
         w = imageWidth img
         h = imageHeight img
         gRow i
            | i >= V.length v       = []
            | (i+1) `rem` w == 0    = asciiReplace (v V.! i) : '\n' :
                                             gRow (i+1)
            | otherwise             = asciiReplace (v V.! i) : gRow (i+1)

-- Convert a dynamic image to grayscale of its image
grayscaleImage :: DynamicImage -> Image Pixel8
grayscaleImage dyImage = case dyImage of
   ImageY8     pix8       -> pix8 -- works
   ImageYA8    pixYA8     -> pixelMap computeLuma pixYA8 -- untested
   ImageRGB8   pixRGB8    -> pixelMap computeLuma pixRGB8 -- works
   ImageRGBA8  pixRGBA8   -> gsRGBA8 pixRGBA8 -- not working
   ImageYCbCr8 pixYCbCr8  -> pixelMap computeLuma pixYCbCr8 -- untested

-- not workisg properly
gsRGBA8 :: Image PixelRGBA8 -> Image Pixel8
gsRGBA8 = pixelMap (\(PixelRGBA8 r g b a) ->
   round (
   (0.21 * (fromIntegral r) +
    0.71 * (fromIntegral g) +
    0.07 * (fromIntegral b) )*(fromIntegral a ) ) )

transRGBA8 :: Image PixelRGBA8 -> Image Pixel8
transRGBA8 img = pixelMap getTransparency img

--transYCbCr8 :: Image PixelYCbCr8 -> Image Pixel8
--transYCbCr8 img = pixelMap getTransparency img

getTransparency :: PixelRGBA8 -> Pixel8
getTransparency (PixelRGBA8 r g b a) = a

shapeToAscii :: CharShape -> Char
shapeToAscii = aSToA . aShape

aShape :: CharShape -> (Quadrant,Quadrant,Quadrant,Quadrant)
aShape (a,b,c,d) = (approx a,approx b,approx c,approx d)

approx :: Word8 -> Quadrant
approx n
   | n >= 150  = Blank
   | n >= 70  = Light
   | otherwise = Dark

-- naive
asciiReplace n
   | n >= 230      = ' '
   | n >= 200      = '.'
   | n >= 170      = '+'
   | n >= 130      = 'o'
   | n >= 100      = 'O'
   | n >= 70       = '#'
   | n >= 40       = 'X'
   | otherwise     = '@'

aSToA :: (Quadrant,Quadrant,Quadrant,Quadrant) -> Char
-- All Blanks
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
