-- John Hurst
-- 2021-12-27

import Data.Array
import Data.Char(digitToInt)
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment

type Ind = (Int, Int)
type HeightMatrix = Array Ind Int

makeHeights :: [Text.Text] -> HeightMatrix
makeHeights lines = listArray ((1,1),(nrows,ncols)) $ concat rows
    where rows = map makeRow lines
          makeRow line = map digitToInt $ Text.unpack line
          nrows = length rows
          ncols = length $ head rows

isLocalMin :: HeightMatrix -> (Ind, Int) -> Bool
isLocalMin a ((r,c), val) = isMinx1 && isMinx2 && isMiny1 && isMiny2
    where ((minRow,minCol),(maxRow,maxCol)) = bounds a
          isMinx1 = r == minRow || val < a!(r-1,c)
          isMinx2 = r == maxRow || val < a!(r+1,c)
          isMiny1 = c == minCol || val < a!(r,c-1)
          isMiny2 = c == maxCol || val < a!(r,c+1)

findBasin' :: [Ind] -> HeightMatrix -> Ind -> [Ind]
findBasin' result a (r,c) = withBelow
    where ((minRow,minCol),(maxRow,maxCol)) = bounds a
          current = (r,c):result
          withAbove = if r == minRow || elem (r-1,c) result || a!(r-1,c) == 9 then current   else findBasin' current a (r-1,c)
          withLeft  = if c == minCol || elem (r,c-1) withAbove || a!(r,c-1) == 9 then withAbove else findBasin' withAbove a (r,c-1)
          withRight = if c == maxCol || elem (r,c+1) withLeft  || a!(r,c+1) == 9 then withLeft  else findBasin' withLeft  a (r,c+1)
          withBelow = if r == maxRow || elem (r+1,c) withRight || a!(r+1,c) == 9 then withRight else findBasin' withRight a (r+1,c)

findBasin :: HeightMatrix -> Ind -> [Ind]
findBasin = findBasin' []

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        heights = makeHeights lines
        minInds = map fst $ filter (isLocalMin heights) $ assocs heights
        basins = map (findBasin heights) minInds
        result = product $ take 3 $ reverse $ sort $ map length basins
    print result
