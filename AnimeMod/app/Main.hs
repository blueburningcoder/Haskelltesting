module Main where

import Anime.Types
import Anime.Files
import Anime.Menu
import System.Environment (getArgs, withArgs)


-- the main function
main = do
  args <- getArgs
  if length args < 1 then putStrLn "Please type an argument too ... [EXEC help]" >> help else do
    case head args of
      "show" -> showAnimeList
      "add" -> addAnime
      "del" -> putStr "Hi there" >> putStr (take 20 $ repeat '\b')
      "edit" -> edit $ tail args
      "help" -> help
      "sort" -> sort
      _ -> putStrLn "This feature is not yet implemented ._."



-- some test-information for not having an empty file
testAllAnime = [ newAnime "Gantz" 212, newAnime "Zetsuen no Tempest" 125]
testComplete = Co [] [] testAllAnime
