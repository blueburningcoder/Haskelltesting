module Main where

import General
import Anime.Types
import Anime.Files
import Anime.Menu
import System.Environment (getArgs, withArgs)

-- the main function
main = do
  loadList
  args <- getArgs
  if length args < 1 then putStrLn "Please type an argument too ... [EXEC help]" >> help else do
    case head args of
      "show" -> showAnimeList
      "add"  -> addNewAnime $ tail args
      "del"  -> delAnime    $ tail args
      "edit" -> edit        $ tail args
      "help" -> help
      "sort" -> sortAllAnime
      "info" -> prompt "Please note that this might take some time. \nDo you wish to continue? " >> undefined
      _ -> putStrLn "This feature is not yet implemented ._."
  resetFiles



-- some test-information for not having an empty file
testAllAnime = [ newAnime "Gantz" 212, newAnime "Zetsuen no Tempest" 125]
testComplete = Co [] [] testAllAnime
