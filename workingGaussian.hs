module GaussianSolver where

import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

type Vector = [Double]
type Matrice = [Vector]
data Line = Line Vector Double 
    deriving (Show, Read, Eq, Ord)
newtype GMatrice = GMatrice { getGMatrice :: [Line] } 
    deriving (Show, Read, Eq, Ord)




gaussian :: GMatrice -> Maybe Vector
gaussian = undefined

oneTheDiagonal :: GMatrice -> GMatrice
oneTheDiagonal = undefined

toGMatrice :: Matrice -> Vector -> GMatrice
toGMatrice = undefined

zeroExceptDiagonal :: GMatrice -> GMatrice
zeroExceptDiagonal m = foldf m b zeroThisValue

foldf :: GMatrice -> [[Int]] -> (GMatrice -> Int -> Int -> GMatrice) -> GMatrice
foldf m [] _ = m
foldf m ([x,y]:c) f
    | x < y = foldf (f m x y) c f
        -- foldf (f m (trace ("\nx: " ++ (show x) ++ " y: " ++ show y) x) y) c f
    | otherwise = 
        foldf m c f

b = [[x,y] | x <- [0..3], y <- [0..3]] :: [[Int]]
-- Ending in 

zeroThisValue :: GMatrice -> Int -> Int -> GMatrice
zeroThisValue a@(GMatrice (b@(Line vec d):(c@(Line veC dd)):l)) x 0 = 
        let factor = calcZeroFactor (vec !! x) (veC !! x)
            newLine = lineSub (lineOp c (*factor)) b
        in insGMat a newLine 0
zeroThisValue (GMatrice (l:li)) x y = 
        let (GMatrice list) = zeroThisValue (GMatrice (li ++ [l])) x (y-1)
        in GMatrice [last list] ++ (init list)

lineSub :: Line -> Line -> Line
lineSub (Line vec d) (Line bec b) = (Line (vecSub vec bec) (d - b))

vecSub :: Vector -> Vector -> Vector
vecSub [] _ = []
vecSub _ [] = []
vecSub (v:vec) (b:bec) = (v - b):(vecSub vec bec)

calcZeroFactor :: Double -> Double -> Double
calcZeroFactor d dd = d / dd 

getZeroFactor :: Line -> Line -> Int -> Line
getZeroFactor (Line vec d) (Line veC dd) i = undefined

zeroBelowTheDiagonal :: GMatrice -> Int -> GMatrice
zeroBelowTheDiagonal (GMatrice (l:li)) i = undefined

fromGMatrice :: GMatrice -> (Matrice, Vector)
fromGMatrice (GMatrice ((Line vec d):li)) = undefined

insGMat :: GMatrice -> Line -> Int -> GMatrice
insGMat (GMatrice (l:li)) nl i 
        | i == 0 = GMatrice (nl:li)
        | otherwise = insGMat (GMatrice li) nl (i - 1)
        

getCol :: GMatrice -> Int -> Maybe Line
getCol (GMatrice []) _ = Nothing
getCol (GMatrice (l:li)) 0 = Just l
getCol (GMatrice (l:li)) i = getCol (GMatrice li) (i-1)

getVal :: GMatrice -> Vector
getVal (GMatrice []) = []
getVal (GMatrice ((Line vec d):li)) = d:(getVal (GMatrice li))

gMatOp :: GMatrice -> (Double -> Double) -> GMatrice
gMatOp (GMatrice []) _ = (GMatrice [])
gMatOp (GMatrice (l:li)) f = GMatrice $ (lineOp l f):(getGMatrice $ gMatOp (GMatrice li) f)

lineOp :: Line -> (Double -> Double) -> Line
lineOp (Line vec d) f = Line (vecOp vec f) (f d)

colOp :: GMatrice -> Int -> (Double -> Double) -> GMatrice
colOp m 0 f = GMatrice $ (lineOp (head . getGMatrice $ m) f):(tail . getGMatrice $ m)
colOp m i f = GMatrice $ (head . getGMatrice $ m):(getGMatrice (colOp (GMatrice (tail . getGMatrice $ m)) (i-1) f))

vecOp :: Vector -> (Double -> Double) -> Vector
vecOp [] _ = []
vecOp (v:vec) f = (f v):(vecOp vec f)


matTest = GMatrice $ (Line [3.0, 1, 4, -1] 7.0):(Line [2.0, -2, -1, 2] 1):(Line [5.0, 7, 14, -8] 20):(Line [1.0, 3, 2, 4] (-4.0)):[]
