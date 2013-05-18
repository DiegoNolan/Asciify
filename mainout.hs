import Codec.Picture
import Codec.Picture.Types
import Data.Word
import Data.Either.Unwrap
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V
import System.IO

--                         TopL   TopR   BotL    BotR
data CharShape = CharShape !Word8 !Word8 !Word8 !Word8 deriving (show)

testGSImage = do
   dy <- readImage "gs.png"
   return $ grayscaleImage (fromRight dy)

type RCord = (Rational, Rational)

heightToWidth :: Rational
heightToWidth = (3/2)

mean :: Fractional a => [a] -> a
mean xs = (sum xs) / (fromIntegral $ length xs)

-- shapes :: (Image Pixel8) -> Int -> [[CharShape]]
-- shapes img w = shapes' img (0,0)
--   where dim = charDims img w

shape :: (Image Pixel8) -> RCord -> RCord -> CharShape
shape img os dim = CharShape (m os hd) (m (hx,snd os) hd)
   (m (fst os, hy) hd) (m (hx,hy) hd)
   where m = meanInRect
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
meanInRect img offset dim = (fromIntegral . floor . mean)  (map (\(x,y) ->
   ((fromIntegral (pixelAt img x y)) *  (area x y)) ) indices)
   where iw = imageWidth img
         ih = imageHeight img
         x1 = fst offset
         x2 = x1 + (fst dim)
         y1 = snd offset
         y2 = x1 + (snd dim)
         xStr = min (floor $ x1) (iw-1)
         yStr = min (floor $ y1) (ih-1)
         xEnd = min (min (ih-1) (floor x2)) (iw-1)
         yEnd = min (min (ih-1) (floor y2)) (iw-1)
         indices = [ (x,y) | x <- [xStr..(min xEnd (iw-1))],
                  y <- [yStr..(min yEnd (ih-1))] ]
         area x y = ((x2' x)-(x1' x))*((y2' y)-(y1' y))
         x1' x = max (fromIntegral x) x1
         x2' x = min (fromIntegral (x+1)) x2
         y1' y = max (fromIntegral y) y1
         y2' y = min (fromIntegral (y+1)) y2

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


