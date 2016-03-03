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
      "add"  -> completelyNew >>= addAnime
      "del"  -> selectAnime "Please enter the name or id of the Anime you wish to delete. : " >>= deleteAnime
      "edit" -> edit $ tail args
      "help" -> help
      "sort" -> prompt "Please Note that this might take some time. \nDo you wish to continue? " >> sortAllAnime -- for some mysterious reason this won't work otherwise.
      _ -> putStrLn "This feature is not yet implemented ._."



-- some test-information for not having an empty file
testAllAnime = [ newAnime "Gantz" 212, newAnime "Zetsuen no Tempest" 125]
testComplete = Co [] [] testAllAnime
