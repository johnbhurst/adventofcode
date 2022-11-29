-- John Hurst
-- 2021-12-27

import Data.List
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment

type CorruptionResult = Maybe Char

beginPair :: Char -> Char
beginPair c
    | c == ')' = '('
    | c == ']' = '['
    | c == '}' = '{'
    | c == '>' = '<'
    | otherwise = error "Unrecognised character in input"

assessCorruption :: [Char] -> [Char] -> CorruptionResult
assessCorruption _ [] = Nothing
assessCorruption stack (c:rest)
    | c `elem` ['(', '[', '{', '<'] = assessCorruption (c:stack) rest
    | c `elem` [')', ']', '}', '>'] = if null stack || beginPair c /=  head stack then Just c else assessCorruption (tail stack) rest
    | otherwise = error "Unrecognised character in input"

assessUnmatched :: [Char] -> [Char] -> [Char]
assessUnmatched stack [] = stack
assessUnmatched stack (c:rest)
    | c `elem` ['(', '[', '{', '<'] = assessUnmatched (c:stack) rest
    | c `elem` [')', ']', '}', '>'] = if null stack || beginPair c /= head stack then error "Unexpected input" else assessUnmatched (tail stack) rest
    | otherwise = error "Unrecognised character in input"

scoreChar :: Char -> Int
scoreChar c
    | c == '(' = 1
    | c == '[' = 2
    | c == '{' = 3
    | c == '<' = 4
    | otherwise = error "Unrecognised character in input"

scoreUnmatched :: [Char] -> Int
scoreUnmatched = foldl accum 0
    where accum result c = 5 * result + scoreChar c

main = do
    [fileName] <- getArgs
    content <- Text.readFile fileName
    let lines = Text.lines content
        input = map Text.unpack lines
        cleanInput = filter (isNothing . assessCorruption []) input
        unmatched = map (assessUnmatched []) cleanInput
        scores = sort $ map scoreUnmatched unmatched
        index = (length scores - 1) `div` 2
        result = scores!!index
    print result
