-- John Hurst
-- 2021-12-17

import Data.List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

type Point = (Int,Int)
type Line = (Point, Point)

toInt :: Text.Text -> Int
toInt = read . Text.unpack . Text.strip

toLine :: Text.Text -> Line
toLine s = ((x1, y1), (x2, y2))
    where [s1, s2] = Text.splitOn (Text.pack "->") s
          [x1, y1] = map toInt $ Text.splitOn (Text.pack ",") s1
          [x2, y2] = map toInt $ Text.splitOn (Text.pack ",") s2

range :: Int -> Int -> [Int]
range v1 v2 = if v1 < v2 then [v1..v2] else [v1,v1-1..v2]

coversPoints :: Line -> [Point]
coversPoints ((x1, y1), (x2, y2))
    | y1 == y2 = map (\x -> (x,y1)) $ range x1 x2
    | x1 == x2 = map (\y -> (x1,y)) $ range y1 y2
    | otherwise = zip (range x1 x2) (range y1 y2)

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = map toLine $ Text.lines content
        points = concatMap coversPoints lines
        result = length $ filter twoOrMore $ group $ sort points
            where twoOrMore l = length l > 1
    print result
