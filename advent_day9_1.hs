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

isLocalMin :: Array (Int,Int) Int -> (Int,Int) -> Bool
isLocalMin a (r,c) = isMinx1 && isMinx2 && isMiny1 && isMiny2
    where ((minRow,minCol),(maxRow,maxCol)) = bounds a
          isMinx1 = r == minRow || a!(r,c) < a!(r-1,c)
          isMinx2 = r == maxRow || a!(r,c) < a!(r+1,c)
          isMiny1 = c == minCol || a!(r,c) < a!(r,c-1)
          isMiny2 = c == maxCol || a!(r,c) < a!(r,c+1)

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        heights = makeHeights lines
        minIndices = filter (isLocalMin heights) $ indices heights
        result = sum $ map ((+1) . (\(r,c) -> heights!(r,c))) minIndices
    print result
