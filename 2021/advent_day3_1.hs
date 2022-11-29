-- John Hurst
-- 2021-12-12

-- Read input into list of list of "0"s and "1"s.
-- Accumulate list of tuples of frequencies of "0"s and "1"s.
-- Transform tuples to selected digits for gamma and epsilon.
-- Convert gamma and espilon digits into decimal numbers.
-- Compute result.

import Data.Char
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

type BinaryDigits = [Bool]
type BinaryDigitFrequencies = [(Int,Int)]

-- ["010", "110", "001"] -> [[False, True, False], [True, True, False], [False, False, True]]
makeBinaryDigits :: [Text.Text] -> [BinaryDigits]
makeBinaryDigits = map (toBools . toChars)
    where toChars = Text.chunksOf 1
          toBools = map toBool
          toBool c = c == Text.pack "1"

-- [[False, True, False], [True, True, False], [False, False, True]] -> [[(1,0), (0,1), (1,0)], [(0,1), (0,1), (1,0)], [(1,0), (1,0), (0,1)]]
makeDigitFrequencies :: [BinaryDigits] -> [BinaryDigitFrequencies]
makeDigitFrequencies = map freqRow
    where freqRow = map freqDigit
          freqDigit b = if b then (0,1) else (1,0)

-- [[(1,0), (0,1), (1,0)], [(0,1), (0,1), (1,0)], [(1,0), (1,0), (0,1)]] -> [(2,1), (1,2), (2,1)]
accumulateDigitFrequencies :: [BinaryDigitFrequencies] -> BinaryDigitFrequencies
accumulateDigitFrequencies = foldl1 accum
    where accum = zipWith combine
          combine (z1, o1) (z2, o2) = (z1+z2, o1+o2)

-- [(2,1), (1,2), (2,1)] -> [True, False, True]
selectDigits :: BinaryDigitFrequencies -> BinaryDigits
selectDigits = map selectDigit
    where selectDigit (zeros, ones) = ones >= zeros

-- [True, False, True] -> 1x4 + 0x2 + 1x1 = 5
toNumber :: BinaryDigits -> Int
toNumber = foldl accum 0
    where accum result bit = 2 * result + if bit then 1 else 0

-- [True, False, True] -> [False, True, False]
complement :: BinaryDigits -> BinaryDigits
complement = map not

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        readings = makeBinaryDigits lines
        digitFrequencies = makeDigitFrequencies readings
        totalFrequencies = accumulateDigitFrequencies digitFrequencies
        gammaDigits = selectDigits totalFrequencies
        gamma = toNumber gammaDigits
        epsilon = toNumber $ complement gammaDigits
        result = gamma * epsilon
    print result
