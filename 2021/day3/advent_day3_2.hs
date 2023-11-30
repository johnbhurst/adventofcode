-- John Hurst
-- 2021-12-14

import Data.Char
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

type BinaryDigits = [Bool]

-- ["010", "110", "001"] -> [[False, True, False], [True, True, False], [False, False, True]]
makeBinaryDigits :: [Text.Text] -> [BinaryDigits]
makeBinaryDigits = map (toBools . toChars)
    where toChars = Text.chunksOf 1
          toBools = map toBool
          toBool c = c == Text.pack "1"

zeroesOnes :: BinaryDigits -> (Int, Int)
zeroesOnes [] = (0, 0)
zeroesOnes (h:t) = if h then (zeroes, ones+1) else (zeroes+1, ones)
    where (zeroes, ones) = zeroesOnes t

selectGammaDigit :: [BinaryDigits] -> Bool
selectGammaDigit digits = ones >= zeroes
    where (zeroes, ones) = zeroesOnes $ map head digits

selectEpsilonDigit :: [BinaryDigits] -> Bool
selectEpsilonDigit digits = ones < zeroes
    where (zeroes, ones) = zeroesOnes $ map head digits

selectDigits :: ([BinaryDigits] -> Bool) -> [BinaryDigits] -> BinaryDigits
selectDigits selectDigit digitsList = headDigit : restDigits
    where headDigit = selectDigit digitsList
          filterDigit digits = head digits == headDigit
          restDigitsList = map tail $ filter filterDigit digitsList
          restDigits = if length restDigitsList == 1
              then head restDigitsList
              else selectDigits selectDigit restDigitsList

selectGammaReading :: [BinaryDigits] -> BinaryDigits
selectGammaReading = selectDigits selectGammaDigit

selectEpsilonReading :: [BinaryDigits] -> BinaryDigits
selectEpsilonReading = selectDigits selectEpsilonDigit

-- [True, False, True] -> 1x4 + 0x2 + 1x1 = 5
toNumber :: BinaryDigits -> Int
toNumber = foldl accum 0
    where accum result bit = 2 * result + if bit then 1 else 0

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        readings = makeBinaryDigits lines
        gammaDigits = selectGammaReading readings
        epsilonDigits = selectEpsilonReading readings
        gamma = toNumber gammaDigits
        epsilon = toNumber epsilonDigits
        result = gamma * epsilon
    print result
