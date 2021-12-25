-- John Hurst
-- 2021-12-27

import Data.Array
import Data.Char(digitToInt)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment

makeHeights :: [Text.Text] -> Array (Int, Int) Int
makeHeights lines = listArray ((1,1),(nrows,ncols)) $ concat rows
    where rows = map makeRow lines
          makeRow line = map digitToInt $ Text.unpack line
          nrows = length rows
          ncols = length $ head rows

isLocalMin :: Array (Int,Int) Int -> ((Int,Int), Int) -> Bool
isLocalMin a ((r,c), val) = isMinx1 && isMinx2 && isMiny1 && isMiny2
    where ((minRow,minCol),(maxRow,maxCol)) = bounds a
          isMinx1 = r == minRow || val < a!(r-1,c)
          isMinx2 = r == maxRow || val < a!(r+1,c)
          isMiny1 = c == minCol || val < a!(r,c-1)
          isMiny2 = c == maxCol || val < a!(r,c+1)

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        heights = makeHeights lines
        minHeights = map snd $ filter (isLocalMin heights) $ assocs heights
        riskLevels = map (+1) minHeights
        result = sum riskLevels
    print result
