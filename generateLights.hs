
import System.IO
import Data.List

data Quadrant = Blank | Light | Dark deriving (Show, Ord, Eq)

allQs = [Blank, Light, Dark]

lst = [ (a,b,c,d) | a <- allQs , b <- allQs , c <- allQs , d <- allQs ]

showQuad (a,b,c,d) = "aSToA (" ++ show a ++ ", " ++ show b ++ ", " ++
      show c ++ ", " ++ show d ++ ") = \' \'\n"

main = do
   handle <- openFile "gen.txt" WriteMode
   hPutStr handle (showLst lst)

showLst [] = ""
showLst (x:xs) = (showQuad x) ++ (showLst xs)

