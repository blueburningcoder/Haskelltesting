
import System.Environment (getArgs)

main = interact addBreaks


addBreaks :: String -> String
addBreaks [] = []
addBreaks (c:s) = if c == ' ' then ' ':'\n':add else c:add
    where add = addBreaks s
