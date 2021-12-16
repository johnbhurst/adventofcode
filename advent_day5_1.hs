-- John Hurst
-- 2021-12-16

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

isHorizontal :: Line -> Bool
isHorizontal ((_,y1), (_,y2)) = y1 == y2

isVertical :: Line -> Bool
isVertical ((x1,_), (x2,_)) = x1 == x2

isSelected :: Line -> Bool
isSelected l = isHorizontal l || isVertical l

range :: Int -> Int -> [Int]
range v1 v2 = if v1 < v2 then [v1..v2] else [v2..v1]

coversPoints :: Line -> [Point]
coversPoints ((x1, y1), (x2, y2))
    | y1 == y2 = map (\x -> (x,y1)) $ range x1 x2
    | x1 == x2 = map (\y -> (x1,y)) $ range y1 y2
    | otherwise = error "Only horizontal or vertical lines supported"

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = filter isSelected $ map toLine $ Text.lines content
        points = concatMap coversPoints lines
        result = length $ filter twoOrMore $ group $ sort points
            where twoOrMore l = length l > 1
    print result
