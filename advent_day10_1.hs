-- John Hurst
-- 2021-12-27

import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment

type ChunkResult = Maybe Char

beginPair :: Char -> Char
beginPair c
    | c == ')' = '('
    | c == ']' = '['
    | c == '}' = '{'
    | c == '>' = '<'
    | otherwise = error "Unrecognised character in input"

assessChars :: [Char] -> [Char] -> ChunkResult
assessChars _ [] = Nothing
assessChars stack (c:rest)
    | c `elem` ['(', '[', '{', '<'] = assessChars (c:stack) rest
    | c `elem` [')', ']', '}', '>'] = if null stack || beginPair c /=  head stack then Just c else assessChars (tail stack) rest
    | otherwise = error "Unrecognised input"

assessLine :: Text.Text -> ChunkResult
assessLine l = assessChars [] $ Text.unpack l

score :: ChunkResult -> Int
score r
    | r == Just ')' = 3
    | r == Just ']' = 57
    | r == Just '}' = 1197
    | r == Just '>' = 25137
    | otherwise = 0

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        chunkResults = map assessLine lines
        scores = map score chunkResults
        result = sum scores
    print result
