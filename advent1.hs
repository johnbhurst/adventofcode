import System.Environment
import System.IO

countLarger :: [Integer] -> Integer
countLarger (h1:h2:t) = (if h2 > h1 then 1 else 0) + countLarger (h2:t)
countLarger l = 0

main = do
    args <- getArgs
    let fileName = head args
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let singleWords = words contents
        ints = map (read::String->Integer) singleWords
        result = countLarger ints
    print result
    hClose handle

