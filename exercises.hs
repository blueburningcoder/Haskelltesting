


{-
 -  n 
 -  Σ k³ = (n(n+1)/2)² 
 - k=1 
 - .
 - mit Vollständiger Induktion
 -}

ksum :: Integral a => a -> a
ksum 0 = 0
ksum k = k^3 + ksum (k-1)


kfunction :: Integral a => a -> a
kfunction n = (n * (n + 1) `div` 2) ^ 2


{-
 - the assumption ksum == kfunction holds for 0, since both results are zero.
 - if we add one each, for the k-function it looks like the following: (prev) m + k³
 - for the kfunction it looks like the following: ((n + 1) * (n + 2) / 2)² , so in our first case for one ofter zero we have 1³ and (1 * 2 / 2)², and that is equally 1. In the next case, 2, we have one plus 2³ for the ksum, resulting in nine. for the kfunction it is (2 * 3 / 2)², resulting in three squared, being nine too. For three it's 1³ + 2³ + 3³, resulting in 36 for the sum, As well as for the function, but resulting out of (3 * 4 / 2)², being 6²
 -
 - Pattern: the sum of the bases below the ^3 is equal to the sole base below the ^2 in the kfunction. Provable?
 -
 - for each and every number in the kfunction, one of them (of the nominator) is odd where the other is even. divided by two we get a 
 -
 -
 -
 - At every step we add either (n + 1)³ to the sum or we add .5n² + .5n + .25
 -}



data Pasc = Pa Int Int

pascal :: Pasc -> Int
pascal (Pa 0 0) = 1
pascal (Pa n k) | k < 0 = 0 | k > n = 0
pascal (Pa n k) = pascal (Pa (n-1) (k-1)) + pascal (Pa (n-1) k)


