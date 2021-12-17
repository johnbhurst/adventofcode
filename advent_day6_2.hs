-- John Hurst
-- 2021-12-18

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

toInt :: Text.Text -> Int
toInt = read . Text.unpack . Text.strip

makePopulation :: [Int] -> [Int] -> [Int]
makePopulation [p0, p1, p2, p3, p4, p5, p6, p7, p8] (timer:t)
    | timer == 1 = makePopulation [p0, p1+1, p2, p3, p4, p5, p6, p7, p8] t
    | timer == 2 = makePopulation [p0, p1, p2+1, p3, p4, p5, p6, p7, p8] t
    | timer == 3 = makePopulation [p0, p1, p2, p3+1, p4, p5, p6, p7, p8] t
    | timer == 4 = makePopulation [p0, p1, p2, p3, p4+1, p5, p6, p7, p8] t
    | timer == 5 = makePopulation [p0, p1, p2, p3, p4, p5+1, p6, p7, p8] t
    | timer == 6 = makePopulation [p0, p1, p2, p3, p4, p5, p6+1, p7, p8] t
    | otherwise = error "Unsupported timer value"
makePopulation l [] = l
makePopulation _ _ = error "Invalid arguments"

nextGeneration :: [Int] -> [Int]
nextGeneration [p0, p1, p2, p3, p4, p5, p6, p7, p8] = [p1, p2, p3, p4, p5, p6, p7+p0, p8, p0]
nextGeneration _ = error "Invalid arguments"

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let line = head $ Text.lines content
        population = makePopulation [0, 0, 0, 0, 0, 0, 0, 0, 0] $ map toInt $ Text.splitOn (Text.pack ",") line
        result = sum $ last $ take 257 $ iterate nextGeneration population
    print result
