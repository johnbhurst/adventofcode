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

type BinaryDigits = [Text.Text]
type BinaryDigitsList = [BinaryDigits]
type BinaryDigitFrequency = (Int, Int)
type BinaryDigitFrequencies = [BinaryDigitFrequency]

-- ["010", "110", "001"] -> [["0", "1", "0"], ["1", "1", "0"], ["0", "0", "1"]]
makeBinaryDigits :: [Text.Text] -> [BinaryDigits]
makeBinaryDigits = map $ Text.chunksOf 1

-- [["0", "1", "0"], ["1", "1", "0"], ["0", "0", "1"]] -> [[(1,0), (0,1), (1,0)], [(0,1), (0,1), (1,0)], [(1,0), (1,0), (0,1)]]
makeDigitFrequencies :: [BinaryDigits] -> [BinaryDigitFrequencies]
makeDigitFrequencies = map freqRow
    where freqRow = map freqDigit
          freqDigit c = if c == Text.pack "0" then (1,0) else (0,1)  

-- [[(1,0), (0,1), (1,0)], [(0,1), (0,1), (1,0)], [(1,0), (1,0), (0,1)]] -> [(2,1), (1,2), (2,1)]
accumulateDigitFrequencies :: [BinaryDigitFrequencies] -> BinaryDigitFrequencies
accumulateDigitFrequencies = foldl1 accum
    where accum = zipWith combine
          combine (z1, o1) (z2, o2) = (z1+z2, o1+o2)

-- [(2,1), (1,2), (2,1)] -> ["1", "0", "1"]
selectDigits :: BinaryDigitFrequencies -> BinaryDigits
selectDigits = map selectDigit
    where selectDigit (zeros, ones) = if zeros > ones then Text.pack "0" else Text.pack "1"

-- ["1", "0", "1"] -> 1x4 + 0x2 + 1x1 = 5
toNumber :: BinaryDigits -> Int
toNumber = foldl accum 0
    where accum result digit = 2 * result + if digit == Text.pack "0" then 0 else 1

-- ["1", "0", "1"] -> ["0", "1", "0"]
complement :: BinaryDigits -> BinaryDigits
complement = map inverse 
    where inverse c = if c == Text.pack "0" then Text.pack "1" else Text.pack "0"

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
