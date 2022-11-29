-- John Hurst
-- 2021-12-18

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

toInt :: Text.Text -> Int
toInt = read . Text.unpack . Text.strip

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let line = head $ Text.lines content
        positions = map toInt $ Text.splitOn (Text.pack ",") line
        minPosition = minimum positions
        maxPosition = maximum positions
        range = [minPosition..maxPosition]
        fuelCosts = map totalCost range
            where totalCost targetPosition = sum $ map (\position -> abs (position - targetPosition)) positions
        result = minimum fuelCosts
    print result
