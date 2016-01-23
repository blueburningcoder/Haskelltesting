
import System.Environment (getArgs)

main = do
    [words] <- getArgs
    intereract 


addBreaks :: String -> String
addBreaks [] = []
addBreaks (c:s) = if c == ' ' then ' ':'\'':'f':add else c:add
    where add = addBreaks s
