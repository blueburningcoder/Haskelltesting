-- | Main module begins HERE :P

module Main where

import           Prelude
import           General
import           Anime.Files.Old
import           Anime.Menu.Old
import           Anime.Stats

import           System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Anime.Files.New as FN
import qualified Anime.Types.New as TN
import qualified Anime.Menu.New  as MN


-- | The main function
main :: IO ()
main = do
  _ <- FN.loadList  -- for later not overwriting unread parts of the file
  args <- getArgs
  if length args < 1 then putStrLn "Please type an argument too ... [EXEC help]" >> help else do
    case head args of
      "show" -> FN.showAnimeList $ tail args
      "add"  -> addNewAnime   $ tail args
      "del"  -> delAnime      $ tail args
      "edit" -> edit          $ tail args
      "seen" -> seen          $ tail args
      "sort" -> sortAllAnime  $ tail args
      "help" -> MN.help
      "info" -> prompt "Please note that this might take some time. \nDo you wish to continue? " >> info
      _ -> putStrLn "This feature is not yet implemented ._."
  resetFiles


