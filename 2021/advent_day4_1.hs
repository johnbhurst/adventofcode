-- John Hurst
-- 2021-12-15

import Data.Char
import Data.List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import System.Environment

type Draw = [Int]
type BoardPosition = (Int, Bool)
type Board = [[BoardPosition]]

toInt :: Text.Text -> Int
toInt = read . Text.unpack

readDraw :: Text.Text -> Draw
readDraw line = map toInt $ Text.splitOn (Text.pack ",") line

makeRow :: Text.Text -> [Int]
makeRow t = map toInt $ Text.words t

makeBoard :: [[Int]] -> Board
makeBoard = map makePositions
    where makePositions = map makePosition
          makePosition i = (i, False)

readBoards' :: [[Int]] -> [Text.Text] -> [Board]
readBoards' rows (h:t) = if h == Text.pack ""
    then makeBoard rows : readBoards' [] t
    else readBoards' (makeRow h : rows) t
readBoards' rows [] = [makeBoard rows | rows /= []]

readBoards :: [Text.Text] -> [Board]
readBoards (h:t) = if h == Text.pack "" then readBoards' [] t else error "Expected blank line after draw"
readBoards [] = error "Expected blank line after draw"

readGame :: [Text.Text] -> (Draw, [Board])
readGame (firstLine:restLines) = (readDraw firstLine, readBoards restLines)
readGame [] = error "Expected input"

place :: Int -> [Board] -> [Board]
place n = map $ placeOnBoard n
    where placeInPosition n (m, f) = if  n == m then (m, True) else (m, f)
          placeOnRow n = map $ placeInPosition n
          placeOnBoard n = map $ placeOnRow n

isFilled :: BoardPosition -> Bool
isFilled = snd

isWin :: Board -> Bool
isWin board = any isRowFilled board || any isRowFilled (transpose board)
    where isRowFilled = all isFilled

findWin :: Draw -> [Board] -> (Int, Board)
findWin (n:restDraws) boards = if winningBoards /= []
    then (n, head winningBoards)
    else findWin restDraws nextBoards
    where nextBoards = place n boards
          winningBoards = filter isWin nextBoards
findWin [] _ = error "No win found"

unfilled :: Board -> [Int]
unfilled board = map fst $ filter (not . isFilled) $ concat board

score :: Board -> Int
score = sum . unfilled

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        (draw, boards) = readGame lines
        (winningNumber, winningBoard) = findWin draw boards
        result = winningNumber * score winningBoard
    print result
