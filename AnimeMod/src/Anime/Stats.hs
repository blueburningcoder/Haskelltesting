-- | This is the module which is going to compute the statistics

module Anime.Stats where

import           Prelude
import           Anime.Files.Old

-- | currently just the information as to how many anime there are in total
info :: IO ()
info = do
  putStr "Total amount of Anime: "
  totalAnimeCount <- getTotalAnime
  putStrLn $ show totalAnimeCount

-- | returns the amount of animes there is in total
getTotalAnime :: IO Int
getTotalAnime = completeList >>= return . length

-- | returns the amount of episodes watched in total (undefined)
getTotalEpWatched :: IO Int
getTotalEpWatched = undefined

