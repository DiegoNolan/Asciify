
import Data.Char
import System.IO
import Data.List

main = do
   handle <- openFile "Generated.hs" WriteMode
   hPutStr handle createMappingFunction

data Brightness = Bnk | Lgt | Drk | Bck deriving (Show, Ord, Eq)

data Novemant = Novemant !Brightness
                         !Brightness
                         !Brightness
                         !Brightness
                         !Brightness
                         !Brightness
                         !Brightness
                         !Brightness
                         !Brightness
                         deriving (Show, Ord, Eq)

allBrightness = [Bnk, Lgt, Drk, Bck]

createMappingFunction =
   foldl' (\acc nov -> (acc ++) $! ("novemantToChar " ++ (show nov) ++ "\t = " ++
   (show $! (bestMatch nov chs)) ++ "\n") ) "" allNov
      where chs = map chr [32..126]
            allNov = createAllNovemants

createAllNovemants = [Novemant a b c d e f g h i |
   a <- allBrightness,
   b <- allBrightness,
   c <- allBrightness,
   d <- allBrightness,
   e <- allBrightness,
   f <- allBrightness,
   g <- allBrightness,
   h <- allBrightness,
   i <- allBrightness ]

bestMatch :: Novemant -> [Char] -> Char
bestMatch n chs = snd $! foldl' (\(mean,char) (mnext, chnext) -> if mnext < mean then
      (mnext,chnext) else (mean,char) ) (10000, ' ') meanAndnov
   where meanAndnov = map (\x -> (meanSquare n (charToNovem x), x)) chs

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

