-- This is supposed to compress stuff to become a dynamic Mesh-net,
-- coarse when possible, but fine when required

-- this is simply a proof-of-concept implementation by me,
-- there certainly are better (faster, smaller, etc.) ones,
-- this one is for me to really understand it and to faster
-- reimplement it if it should be required at some point.
--
--          (needs to be updatet to Vectors)



compress :: (Num a, Ord a) => [a] -> a -> [a]
compress li d = map snd $ compressor d (zip [1..] li)


compressor :: (Num b, Ord b) => b -> [(Integer, b)] -> [(Integer, b)]
-- compressor d li = next (toInteger . sqrt . fromIntegral . fst . last $ li) li
compressor d li = next 18 li
  where
  shor = shortenTris' (\a b c -> abs (a + c) < abs (2 * b - d))
  -- step = connect_triplets $ map shor getTriplets li
--  next :: a -> [(a, b)] -> [(a, b)]
  next 0 a = a
  next n a = next (n-1) (connect_triplets $ map shor $ getTriplets a)


connect_triplets :: [[a]] -> [a]
connect_triplets [a]    = a
connect_triplets (a:li) = init a ++ connect_triplets li


getTriplets :: [a] -> [[a]]
getTriplets (a:b:c:li) = [a,b,c]:getTriplets (c:li)
getTriplets [b, c] = [[b, c]]
getTriplets [c] = [[c]]
getTriplets [] = []


shortenTris' :: (b -> b -> b -> Bool) -> [(a, b)] -> [(a, b)]
shortenTris' f b1@[(a1, a), (_, b), (c1, c)] =
  if f a b c
  then b1
  else [(a1, a), (c1, c)]
shortenTris' _ b1 = b1


-- shortenTris d = shortenTris' (\a b c -> abs b > abs $ (a + c) / 2 + d
shortenTris :: (Ord b, Num b, Fractional b) => b -> [(a, b)] -> [(a, b)]
shortenTris d b1@[(a1, a), (_, b), (c1, c)] =
  if abs ((a + c) / 2 + d) <= abs b
  then [(a1, a), (c1, c)]
  else b1
shortenTris _ e         = e



test_curve  = map (\x -> 0.5*x^3 - 3*x) [-10,-9.75..10]
test_curve2 = map (\x -> x^4 - 2*x^2+x) [-2.5,-2.49..2.5]
test_curve3 = map (\x -> cos x        ) [0,pi/16..2*pi]


main = undefined




--------------------------------------------------------------------------------


-- Embedded Zerotree Wavelet (EZW)
-- data EZW a =



-- Set Partitioning In Hierarchical Trees (SPIHT)
-- data SPIHT a =



-- Wavelet Difference Reduction (WDR), Spatial-orientated Tree Wavelet (STW)
-- data WDR a =



-- 3D-Set Partitioning in hierarchical trees (3D-SPIHT)
-- data 3DSPIHT a =



-- Adaptively Scanned Wavelet Difference Reduction (ASWDR)
-- data ASWDR a =







