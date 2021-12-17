-- John Hurst
-- 2021-12-18

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

toInt :: Text.Text -> Int
toInt = read . Text.unpack . Text.strip

nextGeneration :: [Int] -> [Int]
nextGeneration = concatMap procreate
    where procreate n
            | n == 0 = [6,8]
            | otherwise = [n-1]

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let line = head $ Text.lines content
        population = map toInt $ Text.splitOn (Text.pack ",") line
        result = length $ last $ take 81 $ iterate nextGeneration population
    print result

