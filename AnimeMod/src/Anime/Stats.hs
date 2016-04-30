module Anime.Stats where

import General
import Anime.Types.Old
import Anime.Files
import Anime.Menu

info :: IO ()
info = do
  totalAnimeCount <- getTotalAnime
  putStrLn $ show totalAnimeCount

getTotalAnime :: IO Int
getTotalAnime = completeList >>= return . length

getTotalEpWatched :: IO Int
getTotalEpWatched = undefined

