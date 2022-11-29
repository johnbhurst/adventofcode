-- John Hurst
-- 2021-12-25

import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment

type DigitItem = ([Text.Text], [Text.Text])
type DigitPatterns = (Text.Text, Text.Text, Text.Text, Text.Text, Text.Text, Text.Text, Text.Text, Text.Text, Text.Text, Text.Text)

toInt :: Text.Text -> Int
toInt = read . Text.unpack . Text.strip

toItems :: Text.Text -> DigitItem
toItems line = (patterns, outputs)
    where [left, right] = Text.splitOn (Text.pack "|") line
          patterns = Text.splitOn (Text.pack " ") $ Text.strip left
          outputs = Text.splitOn (Text.pack " ") $ Text.strip right

isLength :: Int -> Text.Text -> Bool
isLength n l = n == Text.length l

findForLength :: Int -> [Text.Text] -> (Text.Text, [Text.Text])
findForLength n patterns = (result, rest)
    where result = head $ filter (isLength n) patterns
          rest = delete result patterns

contains :: Text.Text -> Text.Text -> Bool
contains s ss = Text.all isIn s
    where isIn c = Text.any (==c) ss

same :: Text.Text -> Text.Text -> Bool
same s1 s2 = contains s1 s2 &&  contains s2 s1

find1 = findForLength 2
find7 = findForLength 3
find4 = findForLength 4
find8 = findForLength 7

find3 :: Text.Text -> [Text.Text] -> (Text.Text, [Text.Text])
find3 p7 patterns = (result, rest)
    where result = head $ filter cond patterns
          cond pattern = isLength 5 pattern && contains p7 pattern
          rest = delete result patterns

find6 :: Text.Text -> [Text.Text] -> (Text.Text, [Text.Text])
find6 p7 patterns = (result, rest)
    where result = head $ filter cond patterns
          cond pattern = isLength 6 pattern && (not $ contains p7 pattern)
          rest = delete result patterns

find5 :: Text.Text -> [Text.Text] -> (Text.Text, [Text.Text])
find5 p6 patterns = (result, rest)
    where result = head $ filter cond patterns
          cond pattern = isLength 5 pattern && contains pattern p6
          rest = delete result patterns

find2 = findForLength 5

find9 :: Text.Text -> [Text.Text] -> (Text.Text, [Text.Text])
find9 p5 patterns = (result, rest)
    where result = head $ filter cond patterns
          cond pattern = isLength 6 pattern && contains p5 pattern
          rest = delete result patterns

find0 = head

findPatterns :: [Text.Text] -> DigitPatterns
findPatterns patterns = (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
    where (p1, rest1) = find1 patterns
          (p4, rest4) = find4 rest1
          (p7, rest7) = find7 rest4
          (p8, rest8) = find8 rest7
          (p3, rest3) = find3 p7 rest8
          (p6, rest6) = find6 p7 rest3
          (p5, rest5) = find5 p6 rest6
          (p2, rest2) = find2 rest5
          (p9, rest9) = find9 p5 rest2
          p0 = find0 rest9

calculateOutput :: DigitPatterns -> [Text.Text] -> Text.Text
calculateOutput (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) outputs = Text.concat $ map match outputs
    where match output
             | same output p0 = Text.pack "0"
             | same output p1 = Text.pack "1"
             | same output p2 = Text.pack "2"
             | same output p3 = Text.pack "3"
             | same output p4 = Text.pack "4"
             | same output p5 = Text.pack "5"
             | same output p6 = Text.pack "6"
             | same output p7 = Text.pack "7"
             | same output p8 = Text.pack "8"
             | same output p9 = Text.pack "9"
             | otherwise = error $ concat ["Unrecognised digit pattern [", Text.unpack output, "]"]

findOutput :: DigitItem -> Text.Text
findOutput (patterns, outputs) = calculateOutput (findPatterns patterns) outputs

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        items = map toItems lines
        outputs = map findOutput items
        result = sum $ map toInt outputs
    print result
