import System.Environment
import System.IO

countLargerWindow :: [Integer] -> Integer
countLargerWindow (h1:h2:h3:h4:t) = (if h2 + h3 + h4 > h1 + h2 + h3 then 1 else 0) + countLargerWindow (h2:h3:h4:t)
countLargerWindow l = 0

main = do
    args <- getArgs
    let fileName = head args
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let singleWords = words contents
        ints = map (read::String->Integer) singleWords
        result = countLargerWindow ints
    print result
    hClose handle

