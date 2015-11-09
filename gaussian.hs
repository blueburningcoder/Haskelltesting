module GaussianSolver where

import Data.Maybe


type Vector = [Double]
type Matrice = [Vector]


-- | returns the size of a Matrice
length' :: Matrice -> Int
length' [] = 0
length' ([]:xss) = 1 + length xss
length' ((x:xs):xss) = 1 + length xss


-- | deos a gaussian solver, and if it is solvable, returns the Vector with the result
gaussian :: Matrice -> Vector -> Maybe Vector
gaussian m v 
    | length' m /= length (m !! 0) = Nothing
    | length v /= length (m !! 0) = Nothing
    | otherwise = undefined 




calcOneFactor' :: Matrice -> Int -> Int -> Matrice
calcOneFactor' m x y = matMult m 2.0


-- calculates the Factor needed to make 
calcOneFactor :: Vector -> Int -> Double
calcOneFactor vec c = let n = (c + 1) `mod` (length vec) in recip $ vec !! n

-- | calculating the value to make the n'th and the c'th value the same
-- (e.g. making n zero by subtraction of c)
calcZeroFactor :: Vector -> Int -> Double
calcZeroFactor vec c = let
        n = (c + 1) `mod` length vec
        in vec !! c / vec !! n

-- | Multiplying the whole vector with the double
vecMult :: Vector -> Double -> Vector
vecMult [] _ = []
vecMult (x:xs) m = (m*x):(vecMult xs m)

-- | Multiplying the whole Matrice with the double
matMult :: Matrice -> Double -> Matrice
matMult [] _ = []
matMult (v:mat) m = (vecMult v m):(matMult mat m)

-- | getting only one column from the matrice
colToVec :: Matrice -> Int -> Vector
colToVec [] _ = []
colToVec (v:mat) y = (v !! y):(colToVec mat y)

-- | inserting one Vector in the Matrice on the height of y
insMat :: Matrice -> Vector -> Int -> Matrice
insMat [] _ _ = []
insMat mat [] _ = mat
insMat (m:mat) (v:vec) y = (insVec m y v):(insMat mat vec y)

-- | inserting one element in a vector on height y
insVec :: Vector -> Int -> Double -> Vector
insVec [] _ _ = []
insVec (v:vec) 0 n = (n:vec)
insVec (v:vec) y n = v:(insVec vec (y - 1) n)

-- | Multiplying only one column of the Matrice with the value
colMult :: Matrice -> Int -> Double -> Matrice
colMult = flip (flip.flip (flip (flip.flip colOp) . (*)))
{-
colMult [] _ _ = []
colMult m y f = insMat m (vecMult (colToVec m y) f ) y
-}

-- | does an operation on a whole vector ... like flipped mapping 
vecOp :: Vector -> (Double -> Double) -> Vector
vecOp [] _ = []
vecOp (x:xs) f = (f x):(vecOp xs f)

-- | does an operation on the whole matrice
matOp :: Matrice -> (Double -> Double) -> Matrice
matOp [] _ = []
matOp (m:mat) f = (vecOp m f):(matOp mat f)

-- | does an operation on one column only 
colOp :: Matrice -> Int -> (Double -> Double) -> Matrice
colOp [] _ _ = []
colOp m y f = insMat m (vecOp (colToVec m y) f ) y




testMat = [[3.0, 2, 5, 1]
          ,[1, -2, 7, 3]
          ,[4, -1, 14, 2]
          ,[-1, 2, -8, 4]]

testVec = [7.0, 1, 20, -4]

