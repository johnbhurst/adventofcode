-- John Hurst
-- 2021-12-25

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

type DigitItem = ([Text.Text], [Text.Text])

toItems :: Text.Text -> DigitItem
toItems line = (patterns, outputs)
    where items = Text.splitOn (Text.pack " ") line
          patterns = take 10 items
          outputs = drop 10 items

countUniques :: DigitItem -> Int
countUniques (patterns, outputs) = length $ filter isUnique outputs
    where isUnique output = Text.length output `elem` [2, 3, 4, 7]

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        items = map toItems lines
        result = sum $ map countUniques items
    print result
