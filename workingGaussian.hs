module GaussianSolver where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

type Vector = [Double]
type Matrice = [Vector]
data Line = Line Vector Double 
    deriving (Show, Read, Eq, Ord)
newtype GMatrice = GMatrice { getGMatrice :: [Line] } 
    deriving (Read, Eq, Ord)


instance Show GMatrice where
    show (GMatrice []) = ""
    show (GMatrice (l:li)) = (show l) ++ "\n" ++ show (GMatrice li)


length' :: GMatrice -> Int
length' (GMatrice []) = 0
length' (GMatrice (l:li)) = 1 + (length' (GMatrice li))


gaussian :: GMatrice -> Maybe Vector
gaussian = undefined

-- making all of the diagonal's numbers one
oneTheDiagonal :: GMatrice -> GMatrice
oneTheDiagonal (GMatrice []) = GMatrice []
oneTheDiagonal m = actual 0 m
    where actual n m
            | n >= length' m = m
            | otherwise = actual (n+1) (insGMat m (lineOp (getCol m n) (*(calcOneFactor (getCol m n) n))) n)

-- zeros every value below the diagonal ( except the lowest line, as of yet)
zeroBelowDiagonal :: GMatrice -> GMatrice
zeroBelowDiagonal m = foldf m b zeroThisValue'

-- zeros only the lowest line
zeroLowestLine :: GMatrice -> GMatrice
zeroLowestLine m = undefined -- getFittingLine m (getCol m 3)

n = [[x,y] | x <- [0..3], y <- [3]]

-- returns a 'fitting' line for the given line from the given Matrice
getFittingLine :: GMatrice -> Line -> Line
getFittingLine (GMatrice []) l = (trace "\n\nNO fitting line\n\n") l
getFittingLine (GMatrice (l:lines)) line = if lineFittingNotEqual l line then l else getFittingLine (GMatrice lines) line

-- if the lines are 'fitting' in the sense of having zeros at the same places and are not equal
lineFittingNotEqual :: Line -> Line -> Bool
lineFittingNotEqual (Line vec d) (Line bec b) = (foldfit (d:vec) (b:bec) True) && vec /= bec

-- folds over the vectors 
foldfit :: Vector -> Vector -> Bool -> Bool
foldfit [] _ b = b
foldfit _ [] b = b
foldfit (l:li) (x:xs) b = (foldfit li xs) (b == True && ( (l == 0.0 && x == 0.0) || (l /= 0.0 && x /= 0.0) ) )

-- a fold, implemented due to the need for two variable coming out of the list at each time
foldf :: GMatrice -> [[Int]] -> (GMatrice -> Int -> Int -> GMatrice) -> GMatrice
foldf m [] _ = m
foldf m ([x,y]:c) f
    | x < y = foldf (f m x y) c f
        -- foldf (f m (trace ("\nx: " ++ (show x) ++ " y: " ++ show y) x) y) c f
    | otherwise = foldf m c f

b = [[x,y] | x <- [0..3], y <- [0..3]] :: [[Int]]
-- belowDiag = map (\[x,y] -> if x < y then [x,y] else []) [[x,y] | x <- [0..3], y <- [0..3]] :: [[Int]]
-- b = w ++ [[0,3]]

-- zeroing one explicit value and returning the new Matrice
zeroThisValue :: GMatrice -> Int -> Int -> GMatrice
zeroThisValue a@(GMatrice (b@(Line vec d):(c@(Line veC dd)):l)) x 0 = 
        let factor = calcZeroFactor (vec !! x) (veC !! x)
            newLine = lineSub (lineOp c (*factor)) b
        in insGMat a newLine 0
zeroThisValue (GMatrice (l:li)) x y = 
        let (GMatrice list) = zeroThisValue (GMatrice (li ++ [l])) x (y-1)
        in GMatrice ([last list] ++ (init list))

-- zeroing with explicit search for fitting line (might overwrite some stuff otherwise)
zeroThisValue' :: GMatrice -> Int -> Int -> GMatrice
zeroThisValue' m x 0 = let
        line    =  getCol m 0
        fit     =  getFittingLine m line
        factor  =  calcZeroFactor' line fit x
        newLine =  lineSub (lineOp fit (*factor)) line
        in insGMat m newLine 0
zeroThisValue' (GMatrice (l:li)) x y = 
        let (GMatrice list) = zeroThisValue' (GMatrice (li ++ [l])) x (y-1)
        in GMatrice ([last list] ++ (init list))


-- subtracting one line from the other
lineSub :: Line -> Line -> Line
lineSub (Line vec d) (Line bec b) = (Line (vecSub vec bec) (d - b))

-- subtracting one Vector from the other
vecSub :: Vector -> Vector -> Vector
vecSub [] _ = []
vecSub _ [] = []
vecSub (v:vec) (b:bec) = (v - b):(vecSub vec bec)

-- a -> b -> c => c * b - a = 0
-- the substraicton of the multiplication of this factor with the second parameter with the first parameter is going to be zero -- is this description even correct ?
calcZeroFactor :: Double -> Double -> Double
calcZeroFactor d dd = d / dd 

-- calculating the zero-factor for two lines, based on the x-offset
calcZeroFactor' :: Line -> Line -> Int -> Double
calcZeroFactor' (Line (v:vec) a) (Line (x:xs) b) 0 = calcZeroFactor v x
calcZeroFactor' (Line (v:vec) a) (Line (x:xs) b) i = calcZeroFactor' (Line vec a) (Line xs b) (i-1)
calcZeroFactor' _ _ _ = 0

-- multiplying the resulting factor with the Line results in the x'th number being one
calcOneFactor :: Line -> Int -> Double
calcOneFactor (Line vec d) x = recip (vec !! x)

-- inserts the given Line at the given offset in the given GMatrice
insGMat :: GMatrice -> Line -> Int -> GMatrice
insGMat (GMatrice (l:li)) nl i 
        | i == 0 = GMatrice (nl:li)
        | otherwise = (GMatrice (l:(getGMatrice (insGMat (GMatrice  li) nl (i-1)))))
--         | otherwise = insGMat (GMatrice li) nl (i - 1)
        

-- might return the wanted line. Nothing if the GMatrice is empty
getCol :: GMatrice -> Int -> Line
getCol (GMatrice (l:[])) _ = l
getCol (GMatrice (l:li)) 0 = l
getCol (GMatrice (l:li)) i = getCol (GMatrice li) (i-1)

-- returns a vector of only the resulting values from the linear expression
getVal :: GMatrice -> Vector
getVal (GMatrice []) = []
getVal (GMatrice ((Line vec d):li)) = d:(getVal (GMatrice li))

-- doing an operation over the whole GMatrice
gMatOp :: GMatrice -> (Double -> Double) -> GMatrice
gMatOp (GMatrice []) _ = (GMatrice [])
gMatOp (GMatrice (l:li)) f = GMatrice $ (lineOp l f):(getGMatrice $ gMatOp (GMatrice li) f)

-- doing an operation on the given line
lineOp :: Line -> (Double -> Double) -> Line
lineOp (Line vec d) f = Line (vecOp vec f) (f d)

-- doing an operation on one Line of the Matrice
colOp :: GMatrice -> Int -> (Double -> Double) -> GMatrice
colOp m 0 f = GMatrice $ (lineOp (head . getGMatrice $ m) f):(tail . getGMatrice $ m)
colOp m i f = GMatrice $ (head . getGMatrice $ m):(getGMatrice (colOp (GMatrice (tail . getGMatrice $ m)) (i-1) f))

-- doing an operation on the vector
vecOp :: Vector -> (Double -> Double) -> Vector
vecOp [] _ = []
vecOp (v:vec) f = (f v):(vecOp vec f)

-- testing Matrice
matTest = GMatrice $ (Line [3.0, 1, 4, -1] 7.0):(Line [2.0, -2, -1, 2] 1):(Line [5.0, 7, 14, -8] 20):(Line [1.0, 3, 2, 4] (-4.0)):[]

lineTest = Line [2.0, 6, 2, 1] 3
