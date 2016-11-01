
module Field where

import qualified Data.Vector as V
import qualified Data.Matrix as M


field :: M.Matrix Int
field = M.matrix 10 10 (\_ -> 0)



placeShipVertically :: Int -> M.Matrix Int -> Int -> Int -> M.Matrix Int
placeShipVertically ship = setter ship ship
  where
  setter 0 _ m _ _ = m
  setter left ship mat x y = setter (left -1) ship (safeSet ship (x, y) mat) x (y+1)


placeShipHorizontally :: Int -> M.Matrix Int -> Int -> Int -> M.Matrix Int
placeShipHorizontally ship = setter ship ship
  where
  setter 0 _ m _ _ = m
  setter left ship mat x y = setter (left -1) ship (safeSet ship (x, y) mat) (x+1) y


safeSet :: a -> Int -> Int -> M.Matrix a -> M.Matrix a
safeSet e x y mat =
  if M.nrows mat > y && M.ncols mat > x && x > 0 && y > 0 then undefined else mat -- M.unsafeSet


isPossible :: M.Matrix Int -> Bool
isPossible mat = undefined


placeFirst :: M.Matrix Int -> M.Matrix Int
placeFirst mat = undefined


