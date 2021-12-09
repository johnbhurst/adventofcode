import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

countLargerWindow :: [Integer] -> Integer
countLargerWindow (h1:h2:h3:h4:t) = (if h2+h3+h4 > h1+h2+h3 then 1 else 0) + countLargerWindow (h2:h3:h4:t)
countLargerWindow l = 0

main = do
    fileName <- fmap head getArgs
    lines <- fmap Text.lines (Text.readFile fileName)
    let ints = map toInt lines
            where toInt = read . Text.unpack
        result = countLargerWindow ints
    print result
