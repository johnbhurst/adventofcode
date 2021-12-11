-- John Hurst
-- 2021-12-12

import Data.Char
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

data Command = Forward Int | Up Int | Down Int deriving (Show)
data Position = Position Int Int Int deriving (Show)

update :: Position -> Command -> Position
update (Position h d a) (Forward n) = Position (h+n) (d+a*n) a
update (Position h d a) (Up n) = Position h d (a-n)
update (Position h d a) (Down n) = Position h d (a+n)

result :: Position -> Int
result (Position h d _) = h * d

toInt :: Text.Text -> Int 
toInt s = read $ Text.unpack s

parse :: [Text.Text] -> Command
parse [c,ns] 
    | c == Text.pack "forward" = Forward n 
    | c == Text.pack "up" = Up n
    | c == Text.pack "down" = Down n
    | otherwise = error "Unrecognised command"
    where n = toInt ns
parse _ = error "Expected two items on line"
    
main = do
    fileName <- fmap head getArgs
    lines <- fmap Text.lines (Text.readFile fileName)
    let commands = fmap (parse . Text.split isSpace) lines
        position = foldl update (Position 0 0 0) commands 
    print $ result position
