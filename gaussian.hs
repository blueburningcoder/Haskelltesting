module GaussianSolver where


type Vector = [Double]
type Matrice = [Vector]-- with N x N values


-- | returns the size of a Matrice
length' :: Matrice -> Int
length' [] = 0
length' ([]:xss) = 1 + length xss
length' ((x:xs):xss) = 1 + length xss


-- | does a gaussian solver, and if it is solvable, returns the Vector with the result
gaussian :: Matrice -> Vector -> Maybe Vector
gaussian m v 
    | length' m /= length (m !! 0) = Nothing
    | length v /= length (m !! 0) = Nothing
    | otherwise = undefined 


-- | returns a Vector with factors needed for zeroing p ones below the diagonal
zeroBelowDiagonal :: Matrice -> Int -> Matrice -- anything above one as p makes sense
zeroBelowDiagonal m p = actual (-1)
    where actual c mat p -- actual :: Int -> Matrice -> Int -> [Double]
            | c == length' mat = [] -- p says how many below the diagonal
            | otherwise = do
            let n = (c + 1) `mod` length' mat
                a = (p + n) `mod` length' mat
            (calcZeroFactor (mat !! n) a):(actual n mat p)

-- | returns the Vector one needs to multiply with the Matrice in order to get the diagonal to one's only
oneTheDiagonal :: Matrice -> Vector
oneTheDiagonal [] = []
oneTheDiagonal a@(m:_) = (calcFirstOneFactor m):(oneTheDiagonal . oneLower $ a)

-- returns a Matrice one smaller one number in length on both sides
oneLower :: Matrice -> Matrice
oneLower [] = []
oneLower (m:mat) = withoutFirstElem mat      -- necessary, because we don't need the first column either
    where withoutFirstElem l 
            | l == [] = []
            | otherwise = (tail $ l !! 0):(withoutFirstElem $ tail l)

-- calculates the Factor needed to make the n'th value a one when multiplied with it
calcOneFactor :: Vector -> Int -> Double
calcOneFactor vec c = let n = c `mod` (length vec) in recip $ vec !! n

-- | calculates the one-factor of the first element in the vector
calcFirstOneFactor :: Vector -> Double
calcFirstOneFactor = (flip calcOneFactor) 0

-- | calculating the value to make the n'th and the c'th value the same
-- (e.g. making n zero by subtraction of c)
calcZeroFactor :: Vector -> Int -> Double
calcZeroFactor vec c = let
        l = (c + 1) `mod` length vec
        in vec !! c / vec !! l

-- | Multiplying the whole Vector with the double
vecMult :: Vector -> Double -> Vector
vecMult [] _ = []
vecMult (x:xs) m = (m * x):(vecMult xs m)

-- | multiplies a Vector with another (might be useful later on, having Vectors with factors)
vecMultVec :: Vector -> Vector -> Vector
vecMultVec [] _ = []
vecMultVec _ [] = []
vecMultVec (v:vec) (x:xs) = (v * x):(vecMultVec vec xs)

-- | Multiplying the whole Matrice with the double
matMult :: Matrice -> Double -> Matrice
matMult [] _ = []
matMult (v:mat) m = (vecMult v m):(matMult mat m)


-- Multiplying each row in the Matrice with the respective Value in the Vector
matMultVec :: Matrice -> Vector -> Matrice 
matMultVec [] _ = []
matMultVec _ [] = []
matMultVec (m:mat) (v:vec) = (vecMult m v):(matMultVec mat vec)

-- Multiplying each column in the Matrice with the respective value in the Vector
matMultVec' :: Matrice -> Vector -> Int -> Matrice
matMultVec' [] _ _ = []
matMultVec' _ [] _ = []
matMultVec' mat (v:vec) n = -- init n with 0
        let newMat = insMat mat (vecMult (colToVec mat n) v) n; 
            nextMat = matMultVec' newMat vec ((n + 1) `mod` length' mat)
        in if nextMat == [] then newMat else nextMat


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
colMult = flip $ flip.flip (flip (flip.flip colOp) . (*))
-- WORK OF ART !! Michi -> flipArt :)
-- do you know an easier way? please let me know :)
{-
colMult [] _ _ = []
colMult m y f = insMat m (vecMult (colToVec m y) f ) y
-}

-- | does an operation on a whole Vector ... like flipped mapping 
vecOp :: Vector -> (Double -> Double) -> Vector
vecOp [] _ = []
vecOp (x:xs) f = (f x):(vecOp xs f)

-- | does an operation on the whole Matrice
matOp :: Matrice -> (Double -> Double) -> Matrice
matOp [] _ = []
matOp (m:mat) f = (vecOp m f):(matOp mat f)

-- | does an operation on one column only 
colOp :: Matrice -> Int -> (Double -> Double) -> Matrice
colOp [] _ _ = []
colOp m y f = insMat m (vecOp (colToVec m y) f) y




matTest = [[3.0, 2, 5, 1]
          ,[1, -2, 7, 3]
          ,[4, -1, 14, 2]
          ,[-1, 2, -8, 4]]

vecTest = [7.0, 1, 20, -4]

-- working:
-- matMultVec testMat $ oneTheDiagonal testMat
-- [[1.0,0.6666666666666666,1.6666666666666665,0.3333333333333333],[-0.5,1.0,-3.5,-1.5],[0.2857142857142857,-7.142857142857142e-2,1.0,0.14285714285714285],[-0.25,0.5,-2.0,1.0]]
--
