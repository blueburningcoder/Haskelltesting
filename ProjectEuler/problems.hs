

module Main where

import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe


import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Matrix as M


-- Problem 1

multiples :: Integral a => a -> [a] -> [a]
multiples _  []    = []
multiples m (l:li) 
  | l `mod` m == 0 = l:(multiples m li)
  | otherwise      =    multiples m li


solution :: Int
solution = sum . nub $ [3,5] >>= (flip multiples) [1..999]



-- Problem 2

fibs :: [Integer]
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t


solution2 :: Integer
solution2 = sum $ filter even fibs'
  where fibs' = takeWhile (\a -> a < 4000000) fibs



-- Problem 3

number = 600851475143

max' = ceiling . sqrt . fromIntegral

primeFactors :: Integer -> [Integer]
primeFactors n = [ m | m <- [2..(max' n)], isFactor m n ]

isFactor :: Integer -> Integer -> Bool
isFactor n f = f `mod` n == 0

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = length [ m | m <- [2..(max' n)], isFactor m n] == 0


-- Problem 4

isPalindrome :: String -> Bool
isPalindrome w = w == reverse w

isPalindrome' :: Integer -> Bool
isPalindrome' = isPalindrome . show

solution3 :: Integer
solution3 = maximum $ filter isPalindrome' [ m * n | m <- [100..999], n <- [100..999] ]


-- Problem 5

primes :: [Integer]
primes = [ p | p <- [2..], isPrime p ]

factors :: Integer -> [Integer]
factors n | isPrime n = [n]
factors n = [ m | m <- [2..n], isFactor m n ]

-- solution5 = head $ [s | s <- [2520..], (take 19 $ nub . primeFactors' s) == [2..20] ]

-- incorrect: 38798760, 775975200, 9699690, 193993800, 
-- correct: 232792560 [11,13,14,17,18,19,20]



-- Problem 6

sumSquare :: Integer -> Integer
sumSquare n = (sum $ [1..n]) ^2

squareSum :: Integer -> Integer
squareSum n = sum $ map (^2) [1..n]

diff :: Integer -> Integer
diff n =  sumSquare n - squareSum n



-- Problem 7

-- Problem 8

number8 = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

shorter = filter (\l -> length l >= 13) $ splitOn "0" $ show number8
m       = map parse $ max13 shorter
result  = maximum $ map product m

max13 :: [[a]] -> [[a]]
max13 []     = []
max13 (l:li) = (cSubSets 13 l) ++ (max13 li)


cSubSets :: Int -> [a] -> [[a]]
cSubSets i l | length l < i = []
cSubSets i c@(l:li) = (take i c):(cSubSets i li)

parse :: String -> [Integer]
parse []     = []
parse li = map (fromIntegral . digitToInt) li



-- Problem 9



-- Problem 10

nextPrime :: V.Vector Integer -> Integer -> V.Vector Integer
nextPrime pr m = if next > m then pr else nextPrime (V.cons next pr) m
  where next = nextPrime' pr $ V.last pr + 2

nextPrime' :: V.Vector Integer -> Integer -> Integer
nextPrime' pr n = if isPrime' pr n then n else nextPrime' pr (n+2)

isPrime' :: V.Vector Integer -> Integer -> Bool
isPrime' pr i = V.all (\n -> i `mod` n /= 0) $ V.takeWhile (\b -> b < max) pr
  where max = max' i

-- sol10 = V.sum $ nextPrime (V.fromList [2,3,5]) 2000000
sol10 = V.sum . V.filter isPrime $ V.fromList [3,5..2000000]

some_primes :: IO T.Text
some_primes = T.readFile "primes.txt"


-- Problem 11

input11 :: [[Int]]
input11 = 
  [ [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08]
  , [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00]
  , [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65]
  , [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91]
  , [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80]
  , [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50]
  , [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70]
  , [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21]
  , [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72]
  , [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95]
  , [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92]
  , [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57]
  , [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58]
  , [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40]
  , [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66]
  , [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69]
  , [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36]
  , [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16]
  , [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54]
  , [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]
  ]

input11Vec = V.fromList $ map V.fromList input11 
input11Mat = M.fromLists input11
i = input11Mat

fromMatrixTranspose :: M.Matrix a -> V.Vector (V.Vector a)
fromMatrixTranspose mat = fromMatrix $ M.transpose mat

fromMatrix :: M.Matrix a -> V.Vector (V.Vector a)
fromMatrix mat = V.fromList $ map V.fromList $ M.toLists mat

toMatrix :: V.Vector (V.Vector a) -> M.Matrix a
toMatrix vec = M.fromLists $ V.toList $ V.map V.toList vec

fourAdjecent :: V.Vector (V.Vector Int) -> Int
fourAdjecent vec = fourDiag vec `max` (fourUp vec `max` fourRight vec)


fourRight :: V.Vector (V.Vector Int) -> Int
fourRight vec = V.foldr1 max $ V.map takeFour vec

fourUp    :: V.Vector (V.Vector Int) -> Int
fourUp    vec = fourRight . fromMatrixTranspose . toMatrix $ vec

fourDiag  :: V.Vector (V.Vector Int) -> Int
fourDiag  vec = fourUp mat1 `max` fourRight mat1 `max` fourUp mat2 `max` fourRight mat2
  where mat1 = fromMatrix $ turn1 (toMatrix vec) 0
        mat2 = fromMatrix $ turn2 (toMatrix vec) 0

diagr :: M.Matrix a -> Int -> Int -> Maybe a
diagr mat y x
  | even y    = Nothing
  | otherwise = M.safeGet (x + (y - (M.ncols mat - 1)) `div` 2) (x - (y - (M.ncols mat - 1)) `div` 2) mat

diagu :: M.Matrix a -> Int -> Int -> Maybe a
diagu mat y x
  | odd y     = Nothing
  | otherwise = M.safeGet (x + (M.ncols mat - y) `div` 2) (x + (y - M.ncols mat + 2) `div` 2) mat

turn1 :: M.Matrix a -> a -> M.Matrix a
turn1 mat z = M.matrix (M.nrows mat) (M.ncols mat) (\(a,b) -> fromMaybe z $ diagr mat ((a-1)*2 +1) b)

turn2 :: M.Matrix a -> a -> M.Matrix a
turn2 mat z = M.matrix (M.nrows mat) (M.ncols mat) (\(a,b) -> fromMaybe z $ diagu mat ((a-1)*2) b)

takeFour :: V.Vector Int -> Int
takeFour vec 
  | V.length vec <  4 = 0
  | V.length vec >= 4 = V.product new `max` takeFour (V.tail vec)
  where new = V.take 4 vec




-- Problem 14

collNext :: Integer -> Integer
collNext n 
  | even n    = n `div` 2
  | otherwise = 3 * n + 1


collSeq :: Integer -> [Integer]
collSeq 1 = [1]
collSeq n = [n] ++ collSeq next
  where next = collNext n

sol14 = foldr1 max $ map (\n -> (length . collSeq $ n,n) ) [1..1000000]


-- Problem 16

sumd = (flip sumd2) 0

sumd2 :: Integer -> Integer -> Integer
sumd2 0 acc = acc
sumd2 x acc = sumd2 (x `div` 10) (acc + (x `mod` 10))


main = putStrLn . show $ sol14

