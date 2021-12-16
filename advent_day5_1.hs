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

horizontalPoints :: Line -> [Point]
horizontalPoints ((x1, y1), (x2, y2)) = if x1 < x2 then map pt [x1..x2] else map pt [x2..x1]
    where pt x = (x, y1)

verticalPoints :: Line -> [Point]
verticalPoints ((x1, y1), (x2, y2)) = if y1 < y2 then map pt [y1..y2] else map pt [y2..y1]
    where pt y = (x1, y)

coversPoints :: Line -> [Point]
coversPoints l
    | isHorizontal l = horizontalPoints l
    | isVertical l = verticalPoints l
    | otherwise = error "Only horizontal or vertical lines supported"

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = filter isSelected $ map toLine $ Text.lines content
        points = concatMap coversPoints lines
        result = length $ filter twoOrMore $ group $ sort points
            where twoOrMore l = length l > 1
    print result
