module Main where

import Anime.Types.Old
import Anime.Files.Old
import Anime.Menu.Old

import System.Environment

import Debug.Trace




main = do
  [args] <- getArgs
  file   <- readFile args
  let list = map readLine $ (zip (lines file) [1..])
  mapM_ step list
  -- return $! map (\a -> trace (show a) addAnime a) list
  -- trace (show list) resetFiles
  resetFiles
  where step l = resetFiles >> addAnime l


-- reading a single line, and combines it with the line number as the id
readLine :: (String, Int) -> Anime
readLine (anim, li) = Anime li name NoRating waep toep
  where
  (name, waep, toep) = parse anim
  

-- Parsing a single line
parse :: String -> (String, Episodes, Episodes)
parse ""  = ("", Unknown, Unknown)
parse str = (name, epwa, epto)
  where 
  name  = takeWhile (\c -> c /= ';') str
  begin = dropWhile (\c -> c /= '(') . dropWhile (\c -> c /= ';') $ str
  epwa  = safeEpWa begin
  epto  = safeEpTo begin


safeEpWa :: String -> Episodes
safeEpWa ""  = Unknown
safeEpWa str = readEpisodes $ tail . takeWhile (\c -> c /= '/') $ str 

safeEpTo :: String -> Episodes
safeEpTo ""  = Unknown
safeEpTo str = readEpisodes $ tail . takeWhile (\c -> c /= ')') . dropWhile (\c -> c /= '/') $ str



