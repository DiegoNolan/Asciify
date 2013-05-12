
import Codec.Picture
import Data.Word
import Data.Either.Unwrap
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V
import System.IO

heightToWidth = 1.8312655086848635

penguin = do
    eDImg <- readImage "test.png"
    if isRight eDImg then do
        let dyImg = fromRight eDImg
        let grayscale = grayScaleImage dyImg

        putStrLn "Have Image"
    else putStrLn $ fromLeft eDImg

unsafe = do
    dyImg <- readImage "test.png"
    return $ (\(ImageY8 t) -> t) (fromRight dyImg)

invert :: (Image Pixel8) -> (Image Pixel8)
invert = pixelMap (\p -> (255 - p) :: Word8) 

grayScaleToAscii :: (Image Pixel8) -> String
grayScaleToAscii img = gRow 0
    where v = imageData img
          w = imageWidth img
          h = imageHeight img
          gRow i
            | i >= V.length v       = []
            | (i+1) `rem` w == 0    = asciiReplacement (v V.! i) : '\n' : gRow (i+1)
            | otherwise             = asciiReplacement (v V.! i) : gRow (i+1)
       
testImage = generateImage (\x y -> fromIntegral ((x * y) `div` 9) :: Word8) 50 50

grayScaleImage dyImage = case dyImage of
    ImageY8   imgPixel8   -> Just imgPixel8
    ImageYA8  imgPixelYA8 -> Nothing -- imgPixelYA8
    ImageRGB8 imgPixelRGB8 -> Nothing -- imgPixelRGB8
    ImageRGBA8 imgPixelRGBA8 -> Nothing --imgPixelRGBA8
    ImageYCbCr8 imgPixelYCbCr8 -> Nothing --imgPixelYCbCr8
        

asciiReplacement n
    | n >= 230      = ' '
    | n >= 200      = '.'
    | n >= 170      = '+'
    | n >= 130      = 'o'
    | n >= 100      = 'O'
    | n >= 70       = '#'
    | n >= 40       = 'X'  
    | otherwise     = '@'
    
      
