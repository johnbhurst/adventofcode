import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

countLarger :: [Integer] -> Integer
countLarger (h1:h2:t) = (if h2 > h1 then 1 else 0) + countLarger (h2:t)
countLarger l = 0

main = do
    fileName <- fmap head getArgs
    lines <- fmap Text.lines (Text.readFile fileName)
    let ints = map toInt lines
            where toInt = read . Text.unpack
        result = countLarger ints
    print result
