module Problems1_23 where

import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Debug.Trace (trace)
import Control.Applicative


import qualified Data.Map as Map
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Matrix as M

import qualified Control.Concurrent as A
import qualified Control.Concurrent.Async as A


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
    next (a : t@(b:_) ) = (a+b) : next t


solution2 :: Integer
solution2 = sum $ filter even fibs'
  where fibs' = takeWhile (\a -> a < 4000000) fibs



-- Problem 3

number = 600851475143

max' = ceiling . sqrt . fromIntegral

-- | Returns the prime factors of a Positive (!) Integer.
-- O(sqrt(m)) (?)
primeFactors :: Integer -> [Integer]
primeFactors n = [ m | m <- 2:[3,5..(max' n)], isFactor m n && isPrime m]

-- | actually returns all the prime factors
primeFactors' :: Integer -> [Integer]
primeFactors' n = if length l == 1 then
  if isPrime (l !! 0 `div` n) then
  l ++ [l !! 0 `div` n]
  else l
  else l
  where
  l = [ m | m <- takeMax (max' n) primes4, isFactor m n]


-- | Returns only numbers smaller than this from this
-- possibly infinite list
takeMax :: Integer -> [Integer] -> [Integer]
takeMax n (x:xs)
  | n > x = x:takeMax n xs
  | otherwise = []
takeMax _ [] = []

{-# INLINE isFactor #-}
-- | if it is a factor of this number, O(1).
isFactor :: Integer -> Integer -> Bool
isFactor n f = f `mod` n == 0

-- | Returns True if the given number is a Prime, False otherwise.
-- does this force length to go through more than one element?
-- O(sqrt(m)) (?)
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
  | n >  0 = length (primeFactors' n) == 0
  | n <= 0 = length (primeFactors' (-n)) == 0


-- Problem 4

isPalindrome :: String -> Bool
isPalindrome w = w == reverse w

isPalindrome' :: Integer -> Bool
isPalindrome' = isPalindrome . show

solution3 :: Integer
solution3 = maximum $ filter isPalindrome' [ m * n | m <- [100..999], n <- [100..999] ]


-- Problem 5

primes :: [Integer]
primes = [ p | p <- [2..], isPrime p]

-- | ~0.6614197198105012 percent speedup over 'primes':
-- ( ~30sec, highest: 261'971 / 157'679)
primes1 :: [Integer]
primes1 = 2:3:5:7:[ p | p <- [11,13..], isPrime p ]

-- | ~0.163047890542201 percent faster than 'primes1':
-- ( ~60sec, highest: 633'197 / 544'429)
-- Is it Possible to make it faster through filter ... changes?
primes2 :: [Integer]
primes2 = 2:3:5:7:(filter isPrime $ concat [ [p + 1, p + 3, p + 7, p + 9] | p <- [10,20..]])


sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

-- | slowdown of ~0.5845692282869863 percent compared to 'primes':
-- ( ~30sec, highest: 95'267 / 229'321)
primes3 :: [Integer]
primes3 = sieve [2..]

-- | ~4.438031266809802 improvement compared to 'primes2':
-- ( ~60sec, highest: 3'528'211 / 648'803 )
primes4 :: [Integer]
primes4 = 2 : [ x | x <- [3,5..], isprime x]

isprime x = all (\p -> x `mod` p > 0) (factorsToTry x)
factorsToTry x = takeWhile (\p -> p*p <= x) primes4

-- | ~1.3040287951608596 improvement compared to primes4:
-- (new primes4 is about twice as fast though)
-- ( ~20sec, highest: 2'753'137 / 1'194'923 )
primes5 :: [Integer]
primes5 = sieve2 [2..]

sieve2 xs = sieve' xs Map.empty
  where
  sieve' [] table = []
  sieve' (x:xs) table =
    case Map.lookup x table of
      Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
      Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
      where
        reinsert table prime = Map.insertWith (++) (x+prime) [prime] table


factors :: Integer -> [Integer]
factors n | isPrime n = [n]
factors n = [ m | m <- [1..n], isFactor m n ]

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
m8      = map parse $ max13 shorter
result  = maximum $ map product m8

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
  | otherwise = M.safeGet (x + (y - (M.ncols mat - 1) ) `div` 2) (x - (y - (M.ncols mat - 1) ) `div` 2) mat

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



-- Problem 12

tria :: Integer -> Integer
tria n = n * (n + 1) `div` 2

trias :: [Integer] -> IO [Integer]
trias = A.mapConcurrently (\n -> return $ tria n)

nextTria :: Integer -> Integer -> Integer
nextTria b m = m + (m - b + 1)


-- | n: smaller number who is getting multipled, m: number which multiples must be smaller of and evenly disible it
-- O(m/n)
multiplesDividing :: Integer -> Integer -> [Integer]
multiplesDividing n m = [n * c | c <- [2..maxi], m `mod` (n * c) == 0]
  where maxi = (m `div` n) `div` 2

-- | faster than 'factorsFast' by a magnitude of at least 100'000 (likely way more) in finding all factors
-- for bigger numbers (possibly it's better in the sense of Landau-Notation even)
-- likely O(f) is dependent upon the number of prime factors
factorsFaster :: Integer -> [Integer]
factorsFaster n = [1] ++ factos ++ [n]
  where primes = primeFactors' n
        factos = if length primes > 1 then foldl1 addNotTwice $ map (\p -> factorsFasterSteps p n 1) primes else if length primes == 1 then factorsFasterSteps (primes !! 0) n 1 else []

-- | possibly slightly faster, but crashes for primes bigger 2 !!!
factorsFaster' :: Integer -> [Integer]
factorsFaster' n = [1] ++ factos ++ [n]
  where primes = primeFactors' n
        factos = foldr1 addNotTwice $ map (\p -> factorsFasterSteps p n 1) primes

-- | do not use without exactly knowing what it does. Actual speed-improvement in this case.
-- Warn:
addNotTwice :: Eq a => [a] -> [a] -> [a]
addNotTwice []    lb  = lb
addNotTwice [a]   lb  = a:lb
addNotTwice (a:b:la) lb
  | not . elem a $ lb = addNotTwice la (a:b:lb)
  | otherwise         = addNotTwice la lb

-- | m is some big number, n is a prime-factor evenly dividing it, c is initially 1 and increments up to maxi (m / (2n) )
-- O (m / (2 * n) ) ?
factorsFasterSteps :: Integer -> Integer -> Integer -> [Integer]
factorsFasterSteps n m c
  | mult == reci && m `mod` mult == 0 = [mult]
  | mult >  reci      = []
  | m `mod` mult == 0 = mult : reci : add
  | otherwise         = add
  where maxi = m `div` (n * 2)
        mult = n * c
        reci = m `div` (c * n)
        add  = if c < maxi then factorsFasterSteps n m (c + 1) else []

-- | noticably faster than 'factors' even for small millions
-- likely O(n^2)
factorsFast :: Integer -> [Integer]
factorsFast n = [1] ++ primes ++ multiple ++ {- recips ++ -} [n]
  where primes      = primeFactors' n
        multiple    = nub $ primes >>= (\p -> multiplesDividing p n)
--      recips      = map (n `div`) $ primes ++ multiple

-- | unknown if this really is an improvement
factorsFast' :: Integer -> [Integer]
factorsFast' n = [1] ++ primes ++ (nub $  multiple ++ recips) ++ [n]
  where primes      = primeFactors' n
        multiple    = primes >>= (\p -> multiplesDividing p n)
        recips      = map (n `div`) $ primes ++ multiple

triaMinLength :: Integer -> Integer
triaMinLength n = if (length . factorsFaster' . tria $ n) > 500 then n else triaMinLength (n+1)

pred12 :: Integer -> Bool
pred12 n = ( length . factorsFaster . tria $ n ) >= 500
  where t = tria n

{-
sol12 :: IO ()
sol12 = do A.mapConcurrently (\n -> let t = tria n in
            if pred12 t then putStrLn (show t) else return ()
            ) [10000..]
           return ()
-- there got to be a faster way ...
-}
sol12 :: IO ()
sol12 = do
  putStrLn . show $ triaMinLength 10000





-- Problem 13
input13 =
  [ 37107287533902102798797998220837590246510135740250
  , 46376937677490009712648124896970078050417018260538
  , 74324986199524741059474233309513058123726617309629
  , 91942213363574161572522430563301811072406154908250
  , 23067588207539346171171980310421047513778063246676
  , 89261670696623633820136378418383684178734361726757
  , 28112879812849979408065481931592621691275889832738
  , 44274228917432520321923589422876796487670272189318
  , 47451445736001306439091167216856844588711603153276
  , 70386486105843025439939619828917593665686757934951
  , 62176457141856560629502157223196586755079324193331
  , 64906352462741904929101432445813822663347944758178
  , 92575867718337217661963751590579239728245598838407
  , 58203565325359399008402633568948830189458628227828
  , 80181199384826282014278194139940567587151170094390
  , 35398664372827112653829987240784473053190104293586
  , 86515506006295864861532075273371959191420517255829
  , 71693888707715466499115593487603532921714970056938
  , 54370070576826684624621495650076471787294438377604
  , 53282654108756828443191190634694037855217779295145
  , 36123272525000296071075082563815656710885258350721
  , 45876576172410976447339110607218265236877223636045
  , 17423706905851860660448207621209813287860733969412
  , 81142660418086830619328460811191061556940512689692
  , 51934325451728388641918047049293215058642563049483
  , 62467221648435076201727918039944693004732956340691
  , 15732444386908125794514089057706229429197107928209
  , 55037687525678773091862540744969844508330393682126
  , 18336384825330154686196124348767681297534375946515
  , 80386287592878490201521685554828717201219257766954
  , 78182833757993103614740356856449095527097864797581
  , 16726320100436897842553539920931837441497806860984
  , 48403098129077791799088218795327364475675590848030
  , 87086987551392711854517078544161852424320693150332
  , 59959406895756536782107074926966537676326235447210
  , 69793950679652694742597709739166693763042633987085
  , 41052684708299085211399427365734116182760315001271
  , 65378607361501080857009149939512557028198746004375
  , 35829035317434717326932123578154982629742552737307
  , 94953759765105305946966067683156574377167401875275
  , 88902802571733229619176668713819931811048770190271
  , 25267680276078003013678680992525463401061632866526
  , 36270218540497705585629946580636237993140746255962
  , 24074486908231174977792365466257246923322810917141
  , 91430288197103288597806669760892938638285025333403
  , 34413065578016127815921815005561868836468420090470
  , 23053081172816430487623791969842487255036638784583
  , 11487696932154902810424020138335124462181441773470
  , 63783299490636259666498587618221225225512486764533
  , 67720186971698544312419572409913959008952310058822
  , 95548255300263520781532296796249481641953868218774
  , 76085327132285723110424803456124867697064507995236
  , 37774242535411291684276865538926205024910326572967
  , 23701913275725675285653248258265463092207058596522
  , 29798860272258331913126375147341994889534765745501
  , 18495701454879288984856827726077713721403798879715
  , 38298203783031473527721580348144513491373226651381
  , 34829543829199918180278916522431027392251122869539
  , 40957953066405232632538044100059654939159879593635
  , 29746152185502371307642255121183693803580388584903
  , 41698116222072977186158236678424689157993532961922
  , 62467957194401269043877107275048102390895523597457
  , 23189706772547915061505504953922979530901129967519
  , 86188088225875314529584099251203829009407770775672
  , 11306739708304724483816533873502340845647058077308
  , 82959174767140363198008187129011875491310547126581
  , 97623331044818386269515456334926366572897563400500
  , 42846280183517070527831839425882145521227251250327
  , 55121603546981200581762165212827652751691296897789
  , 32238195734329339946437501907836945765883352399886
  , 75506164965184775180738168837861091527357929701337
  , 62177842752192623401942399639168044983993173312731
  , 32924185707147349566916674687634660915035914677504
  , 99518671430235219628894890102423325116913619626622
  , 73267460800591547471830798392868535206946944540724
  , 76841822524674417161514036427982273348055556214818
  , 97142617910342598647204516893989422179826088076852
  , 87783646182799346313767754307809363333018982642090
  , 10848802521674670883215120185883543223812876952786
  , 71329612474782464538636993009049310363619763878039
  , 62184073572399794223406235393808339651327408011116
  , 66627891981488087797941876876144230030984490851411
  , 60661826293682836764744779239180335110989069790714
  , 85786944089552990653640447425576083659976645795096
  , 66024396409905389607120198219976047599490197230297
  , 64913982680032973156037120041377903785566085089252
  , 16730939319872750275468906903707539413042652315011
  , 94809377245048795150954100921645863754710598436791
  , 78639167021187492431995700641917969777599028300699
  , 15368713711936614952811305876380278410754449733078
  , 40789923115535562561142322423255033685442488917353
  , 44889911501440648020369068063960672322193204149535
  , 41503128880339536053299340368006977710650566631954
  , 81234880673210146739058568557934581403627822703280
  , 82616570773948327592232845941706525094512325230608
  , 22918802058777319719839450180888072429661980811197
  , 77158542502016545090413245809786882778948721859617
  , 72107838435069186155435662884062257473692284509516
  , 20849603980134001723930671666823555245252804609722
  , 53503534226472524250874054075591789781264330331690
  ]


sol13 = do
  V.sum $ V.fromList input13



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



-- Problem 15

{-
unsigned binomial(unsigned n, unsigned k) {
  unsigned c = 1, i;
  if (k > n-k) k = n-k;  /* take advantage of symmetry */
  for (i = 1; i <= k; i++, n--) {
    c = c/i * n + c%i * n / i;
    /* split c*n/i into (c/i*i + c%i)*n/i */
  }
  return c;
}
-}

binominal :: Integer -> Integer -> Integer
binominal n k = loop n 1 1
  where
  k' = if k > n - k then n - k else k
  loop n i c
    | i <= k' = loop (n-1) (i+1) (c `div` i * n + c `mod` i * n `div` i)
    | otherwise = c


-- Problem 16

sumd = (flip sumd2) 0

sumd2 :: Integer -> Integer -> Integer
sumd2 0 acc = acc
sumd2 x acc = sumd2 (x `div` 10) (acc + (x `mod` 10) )



-- Problem 17

words17 =
  [ ""          -- 0
  , "one"       -- 1
  , "two"       -- 2
  , "three"     -- 3
  , "four"      -- 4
  , "five"      -- 5
  , "six"       -- 6
  , "seven"     -- 7
  , "eight"     -- 8
  , "nine"      -- 9
  , "ten"       -- 10
  , "eleven"    -- 11
  , "twelve"    -- 12

  , "teen"      -- 13
  , "ty"        -- 14

  , "twen"      -- 15
  , "thir"      -- 16
  , "for"       -- 17
  , "fif"       -- 18
  , "six"       -- 19
  , "seven"     -- 20
  , "eigh"      -- 21
  , "nine"      -- 22

  , "hundred"   -- 23
  , "thousand"  -- 24
  , "and"       -- 25
  ]

write :: Int -> String
write n
  | n < 13    = words17 !! n
  | n == 14   = words17 !! 4 ++ words17 !! 13
  | n < 20    = words17 !! (n + 3 ) ++ words17 !! 13
  | n < 100   = words17 !! (d + 13) ++ words17 !! 14 ++ if m  > 0 then "-" ++ write m else ""
  | n < 1000  = words17 !! h ++ " " ++ words17 !! 23 ++ if mh > 0 then " and " ++ write mh else ""
  | n < 10000 = words17 !! t ++ " " ++ words17 !! 24 ++ if th > 0 then " " ++ write th else ""
  | otherwise = "number is of no concern for me"

  where d = n `div` 10; m = n `mod` 10; h = n `div` 100; mh = n `mod` 100; t = n `div` 1000; th = n `mod` 1000



-- Problem 18

type Path = [Bool] -- if you go right on your path - default ~= Left
type Loc  = (Int, Int)

input18 =
  [ [ 75 ]
  , [ 95, 64 ]
  , [ 17, 47, 82 ]
  , [ 18, 35, 87, 10 ]
  , [ 20, 04, 82, 47, 65 ]
  , [ 19, 01, 23, 75, 03, 34 ]
  , [ 88, 02, 77, 73, 07, 63, 67 ]
  , [ 99, 65, 04, 28, 06, 16, 70, 92 ]
  , [ 41, 41, 26, 56, 83, 40, 80, 70, 33 ]
  , [ 41, 48, 72, 33, 47, 32, 37, 16, 94, 29 ]
  , [ 53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14 ]
  , [ 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57 ]
  , [ 91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48 ]
  , [ 63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31 ]
  , [ 04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23 ] ]


sumPath :: Path -> Int
sumPath = sum . getPath

getPath :: Path -> [Int]
getPath []       = [getItem (0, 0)]
getPath c@(p:pa) = (getPath pa) ++ [i]
  where i = getItem (toLoc c)

toLoc :: Path -> Loc
toLoc []     = (0, 0)
toLoc (p:pa) = if p then (x + 1, y + 1) else (x, y + 1)
  where (x, y) = toLoc pa

getItem :: Loc -> Int
getItem (x, y) = (input18 !! y) !! x

minSum :: Int
minSum = sum . getPath . take 14 $ repeat False


-- Problem 19
-- > 1 Jan 1900 was a Monday.
-- > Thirty days has September,
--   April, June and November.
--   All the rest have thirty-one,
--   Saving February alone,
--   Which has twenty-eight, rain or shine.
--   And on leap years, twenty-nine.
-- > A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

year =
  [ 31 -- Jan
  , 28 -- Feb
  , 31 -- Mar
  , 30 -- Apr
  , 31 -- Mai
  , 30 -- Jun
  , 31 -- Jul
  , 31 -- Aug
  , 30 -- Sep
  , 31 -- Okt
  , 30 -- Now
  , 31 -- Dez
  ]

days =
  [ "Monday"
  , "Tuesday"
  , "Wednesday"
  , "Thursday"
  , "Friday"
  , "Saturday"
  , "Sunday"
  ]

getDay :: Int -> String
getDay n = days !! ((n - 1) `mod` 7)

isSunday :: Int -> Bool
isSunday n = n `mod` 7 == 0









-- Problem 20

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

num = "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864"







-- Problem 21
-- the sum of all amicable pairs below 10000

d :: Integer -> Integer
d = sum . init . factorsFaster


isAmicable :: Integer -> Bool
isAmicable a = a == a' && a /= b
  where b  = d a
        a' = d b



-- Problem 22




-- Problem 23

names = V.fromList $ sort [ "MARY","PATRICIA","LINDA","BARBARA","ELIZABETH","JENNIFER","MARIA","SUSAN","MARGARET","DOROTHY","LISA","NANCY","KAREN","BETTY","HELEN","SANDRA","DONNA","CAROL","RUTH","SHARON","MICHELLE","LAURA","SARAH","KIMBERLY","DEBORAH","JESSICA","SHIRLEY","CYNTHIA","ANGELA","MELISSA","BRENDA","AMY","ANNA","REBECCA","VIRGINIA","KATHLEEN","PAMELA","MARTHA","DEBRA","AMANDA","STEPHANIE","CAROLYN","CHRISTINE","MARIE","JANET","CATHERINE","FRANCES","ANN","JOYCE","DIANE","ALICE","JULIE","HEATHER","TERESA","DORIS","GLORIA","EVELYN","JEAN","CHERYL","MILDRED","KATHERINE","JOAN","ASHLEY","JUDITH","ROSE","JANICE","KELLY","NICOLE","JUDY","CHRISTINA","KATHY","THERESA","BEVERLY","DENISE","TAMMY","IRENE","JANE","LORI","RACHEL","MARILYN","ANDREA","KATHRYN","LOUISE","SARA","ANNE","JACQUELINE","WANDA","BONNIE","JULIA","RUBY","LOIS","TINA","PHYLLIS","NORMA","PAULA","DIANA","ANNIE","LILLIAN","EMILY","ROBIN","PEGGY","CRYSTAL","GLADYS","RITA","DAWN","CONNIE","FLORENCE","TRACY","EDNA","TIFFANY","CARMEN","ROSA","CINDY","GRACE","WENDY","VICTORIA","EDITH","KIM","SHERRY","SYLVIA","JOSEPHINE","THELMA","SHANNON","SHEILA","ETHEL","ELLEN","ELAINE","MARJORIE","CARRIE","CHARLOTTE","MONICA","ESTHER","PAULINE","EMMA","JUANITA","ANITA","RHONDA","HAZEL","AMBER","EVA","DEBBIE","APRIL","LESLIE","CLARA","LUCILLE","JAMIE","JOANNE","ELEANOR","VALERIE","DANIELLE","MEGAN","ALICIA","SUZANNE","MICHELE","GAIL","BERTHA","DARLENE","VERONICA","JILL","ERIN","GERALDINE","LAUREN","CATHY","JOANN","LORRAINE","LYNN","SALLY","REGINA","ERICA","BEATRICE","DOLORES","BERNICE","AUDREY","YVONNE","ANNETTE","JUNE","SAMANTHA","MARION","DANA","STACY","ANA","RENEE","IDA","VIVIAN","ROBERTA","HOLLY","BRITTANY","MELANIE","LORETTA","YOLANDA","JEANETTE","LAURIE","KATIE","KRISTEN","VANESSA","ALMA","SUE","ELSIE","BETH","JEANNE","VICKI","CARLA","TARA","ROSEMARY","EILEEN","TERRI","GERTRUDE","LUCY","TONYA","ELLA","STACEY","WILMA","GINA","KRISTIN","JESSIE","NATALIE","AGNES","VERA","WILLIE","CHARLENE","BESSIE","DELORES","MELINDA","PEARL","ARLENE","MAUREEN","COLLEEN","ALLISON","TAMARA","JOY","GEORGIA","CONSTANCE","LILLIE","CLAUDIA","JACKIE","MARCIA","TANYA","NELLIE","MINNIE","MARLENE","HEIDI","GLENDA","LYDIA","VIOLA","COURTNEY","MARIAN","STELLA","CAROLINE","DORA","JO","VICKIE","MATTIE","TERRY","MAXINE","IRMA","MABEL","MARSHA","MYRTLE","LENA","CHRISTY","DEANNA","PATSY","HILDA","GWENDOLYN","JENNIE","NORA","MARGIE","NINA","CASSANDRA","LEAH","PENNY","KAY","PRISCILLA","NAOMI","CAROLE","BRANDY","OLGA","BILLIE","DIANNE","TRACEY","LEONA","JENNY","FELICIA","SONIA","MIRIAM","VELMA","BECKY","BOBBIE","VIOLET","KRISTINA","TONI","MISTY","MAE","SHELLY","DAISY","RAMONA","SHERRI","ERIKA","KATRINA","CLAIRE","LINDSEY","LINDSAY","GENEVA","GUADALUPE","BELINDA","MARGARITA","SHERYL","CORA","FAYE","ADA","NATASHA","SABRINA","ISABEL","MARGUERITE","HATTIE","HARRIET","MOLLY","CECILIA","KRISTI","BRANDI","BLANCHE","SANDY","ROSIE","JOANNA","IRIS","EUNICE","ANGIE","INEZ","LYNDA","MADELINE","AMELIA","ALBERTA","GENEVIEVE","MONIQUE","JODI","JANIE","MAGGIE","KAYLA","SONYA","JAN","LEE","KRISTINE","CANDACE","FANNIE","MARYANN","OPAL","ALISON","YVETTE","MELODY","LUZ","SUSIE","OLIVIA","FLORA","SHELLEY","KRISTY","MAMIE","LULA","LOLA","VERNA","BEULAH","ANTOINETTE","CANDICE","JUANA","JEANNETTE","PAM","KELLI","HANNAH","WHITNEY","BRIDGET","KARLA","CELIA","LATOYA","PATTY","SHELIA","GAYLE","DELLA","VICKY","LYNNE","SHERI","MARIANNE","KARA","JACQUELYN","ERMA","BLANCA","MYRA","LETICIA","PAT","KRISTA","ROXANNE","ANGELICA","JOHNNIE","ROBYN","FRANCIS","ADRIENNE","ROSALIE","ALEXANDRA","BROOKE","BETHANY","SADIE","BERNADETTE","TRACI","JODY","KENDRA","JASMINE","NICHOLE","RACHAEL","CHELSEA","MABLE","ERNESTINE","MURIEL","MARCELLA","ELENA","KRYSTAL","ANGELINA","NADINE","KARI","ESTELLE","DIANNA","PAULETTE","LORA","MONA","DOREEN","ROSEMARIE","ANGEL","DESIREE","ANTONIA","HOPE","GINGER","JANIS","BETSY","CHRISTIE","FREDA","MERCEDES","MEREDITH","LYNETTE","TERI","CRISTINA","EULA","LEIGH","MEGHAN","SOPHIA","ELOISE","ROCHELLE","GRETCHEN","CECELIA","RAQUEL","HENRIETTA","ALYSSA","JANA","KELLEY","GWEN","KERRY","JENNA","TRICIA","LAVERNE","OLIVE","ALEXIS","TASHA","SILVIA","ELVIRA","CASEY","DELIA","SOPHIE","KATE","PATTI","LORENA","KELLIE","SONJA","LILA","LANA","DARLA","MAY","MINDY","ESSIE","MANDY","LORENE","ELSA","JOSEFINA","JEANNIE","MIRANDA","DIXIE","LUCIA","MARTA","FAITH","LELA","JOHANNA","SHARI","CAMILLE","TAMI","SHAWNA","ELISA","EBONY","MELBA","ORA","NETTIE","TABITHA","OLLIE","JAIME","WINIFRED","KRISTIE","MARINA","ALISHA","AIMEE","RENA","MYRNA","MARLA","TAMMIE","LATASHA","BONITA","PATRICE","RONDA","SHERRIE","ADDIE","FRANCINE","DELORIS","STACIE","ADRIANA","CHERI","SHELBY","ABIGAIL","CELESTE","JEWEL","CARA","ADELE","REBEKAH","LUCINDA","DORTHY","CHRIS","EFFIE","TRINA","REBA","SHAWN","SALLIE","AURORA","LENORA","ETTA","LOTTIE","KERRI","TRISHA","NIKKI","ESTELLA","FRANCISCA","JOSIE","TRACIE","MARISSA","KARIN","BRITTNEY","JANELLE","LOURDES","LAUREL","HELENE","FERN","ELVA","CORINNE","KELSEY","INA","BETTIE","ELISABETH","AIDA","CAITLIN","INGRID","IVA","EUGENIA","CHRISTA","GOLDIE","CASSIE","MAUDE","JENIFER","THERESE","FRANKIE","DENA","LORNA","JANETTE","LATONYA","CANDY","MORGAN","CONSUELO","TAMIKA","ROSETTA","DEBORA","CHERIE","POLLY","DINA","JEWELL","FAY","JILLIAN","DOROTHEA","NELL","TRUDY","ESPERANZA","PATRICA","KIMBERLEY","SHANNA","HELENA","CAROLINA","CLEO","STEFANIE","ROSARIO","OLA","JANINE","MOLLIE","LUPE","ALISA","LOU","MARIBEL","SUSANNE","BETTE","SUSANA","ELISE","CECILE","ISABELLE","LESLEY","JOCELYN","PAIGE","JONI","RACHELLE","LEOLA","DAPHNE","ALTA","ESTER","PETRA","GRACIELA","IMOGENE","JOLENE","KEISHA","LACEY","GLENNA","GABRIELA","KERI","URSULA","LIZZIE","KIRSTEN","SHANA","ADELINE","MAYRA","JAYNE","JACLYN","GRACIE","SONDRA","CARMELA","MARISA","ROSALIND","CHARITY","TONIA","BEATRIZ","MARISOL","CLARICE","JEANINE","SHEENA","ANGELINE","FRIEDA","LILY","ROBBIE","SHAUNA","MILLIE","CLAUDETTE","CATHLEEN","ANGELIA","GABRIELLE","AUTUMN","KATHARINE","SUMMER","JODIE","STACI","LEA","CHRISTI","JIMMIE","JUSTINE","ELMA","LUELLA","MARGRET","DOMINIQUE","SOCORRO","RENE","MARTINA","MARGO","MAVIS","CALLIE","BOBBI","MARITZA","LUCILE","LEANNE","JEANNINE","DEANA","AILEEN","LORIE","LADONNA","WILLA","MANUELA","GALE","SELMA","DOLLY","SYBIL","ABBY","LARA","DALE","IVY","DEE","WINNIE","MARCY","LUISA","JERI","MAGDALENA","OFELIA","MEAGAN","AUDRA","MATILDA","LEILA","CORNELIA","BIANCA","SIMONE","BETTYE","RANDI","VIRGIE","LATISHA","BARBRA","GEORGINA","ELIZA","LEANN","BRIDGETTE","RHODA","HALEY","ADELA","NOLA","BERNADINE","FLOSSIE","ILA","GRETA","RUTHIE","NELDA","MINERVA","LILLY","TERRIE","LETHA","HILARY","ESTELA","VALARIE","BRIANNA","ROSALYN","EARLINE","CATALINA","AVA","MIA","CLARISSA","LIDIA","CORRINE","ALEXANDRIA","CONCEPCION","TIA","SHARRON","RAE","DONA","ERICKA","JAMI","ELNORA","CHANDRA","LENORE","NEVA","MARYLOU","MELISA","TABATHA","SERENA","AVIS","ALLIE","SOFIA","JEANIE","ODESSA","NANNIE","HARRIETT","LORAINE","PENELOPE","MILAGROS","EMILIA","BENITA","ALLYSON","ASHLEE","TANIA","TOMMIE","ESMERALDA","KARINA","EVE","PEARLIE","ZELMA","MALINDA","NOREEN","TAMEKA","SAUNDRA","HILLARY","AMIE","ALTHEA","ROSALINDA","JORDAN","LILIA","ALANA","GAY","CLARE","ALEJANDRA","ELINOR","MICHAEL","LORRIE","JERRI","DARCY","EARNESTINE","CARMELLA","TAYLOR","NOEMI","MARCIE","LIZA","ANNABELLE","LOUISA","EARLENE","MALLORY","CARLENE","NITA","SELENA","TANISHA","KATY","JULIANNE","JOHN","LAKISHA","EDWINA","MARICELA","MARGERY","KENYA","DOLLIE","ROXIE","ROSLYN","KATHRINE","NANETTE","CHARMAINE","LAVONNE","ILENE","KRIS","TAMMI","SUZETTE","CORINE","KAYE","JERRY","MERLE","CHRYSTAL","LINA","DEANNE","LILIAN","JULIANA","ALINE","LUANN","KASEY","MARYANNE","EVANGELINE","COLETTE","MELVA","LAWANDA","YESENIA","NADIA","MADGE","KATHIE","EDDIE","OPHELIA","VALERIA","NONA","MITZI","MARI","GEORGETTE","CLAUDINE","FRAN","ALISSA","ROSEANN","LAKEISHA","SUSANNA","REVA","DEIDRE","CHASITY","SHEREE","CARLY","JAMES","ELVIA","ALYCE","DEIRDRE","GENA","BRIANA","ARACELI","KATELYN","ROSANNE","WENDI","TESSA","BERTA","MARVA","IMELDA","MARIETTA","MARCI","LEONOR","ARLINE","SASHA","MADELYN","JANNA","JULIETTE","DEENA","AURELIA","JOSEFA","AUGUSTA","LILIANA","YOUNG","CHRISTIAN","LESSIE","AMALIA","SAVANNAH","ANASTASIA","VILMA","NATALIA","ROSELLA","LYNNETTE","CORINA","ALFREDA","LEANNA","CAREY","AMPARO","COLEEN","TAMRA","AISHA","WILDA","KARYN","CHERRY","QUEEN","MAURA","MAI","EVANGELINA","ROSANNA","HALLIE","ERNA","ENID","MARIANA","LACY","JULIET","JACKLYN","FREIDA","MADELEINE","MARA","HESTER","CATHRYN","LELIA","CASANDRA","BRIDGETT","ANGELITA","JANNIE","DIONNE","ANNMARIE","KATINA","BERYL","PHOEBE","MILLICENT","KATHERYN","DIANN","CARISSA","MARYELLEN","LIZ","LAURI","HELGA","GILDA","ADRIAN","RHEA","MARQUITA","HOLLIE","TISHA","TAMERA","ANGELIQUE","FRANCESCA","BRITNEY","KAITLIN","LOLITA","FLORINE","ROWENA","REYNA","TWILA","FANNY","JANELL","INES","CONCETTA","BERTIE","ALBA","BRIGITTE","ALYSON","VONDA","PANSY","ELBA","NOELLE","LETITIA","KITTY","DEANN","BRANDIE","LOUELLA","LETA","FELECIA","SHARLENE","LESA","BEVERLEY","ROBERT","ISABELLA","HERMINIA","TERRA","CELINA","TORI","OCTAVIA","JADE","DENICE","GERMAINE","SIERRA","MICHELL","CORTNEY","NELLY","DORETHA","SYDNEY","DEIDRA","MONIKA","LASHONDA","JUDI","CHELSEY","ANTIONETTE","MARGOT","BOBBY","ADELAIDE","NAN","LEEANN","ELISHA","DESSIE","LIBBY","KATHI","GAYLA","LATANYA","MINA","MELLISA","KIMBERLEE","JASMIN","RENAE","ZELDA","ELDA","MA","JUSTINA","GUSSIE","EMILIE","CAMILLA","ABBIE","ROCIO","KAITLYN","JESSE","EDYTHE","ASHLEIGH","SELINA","LAKESHA","GERI","ALLENE","PAMALA","MICHAELA","DAYNA","CARYN","ROSALIA","SUN","JACQULINE","REBECA","MARYBETH","KRYSTLE","IOLA","DOTTIE","BENNIE","BELLE","AUBREY","GRISELDA","ERNESTINA","ELIDA","ADRIANNE","DEMETRIA","DELMA","CHONG","JAQUELINE","DESTINY","ARLEEN","VIRGINA","RETHA","FATIMA","TILLIE","ELEANORE","CARI","TREVA","BIRDIE","WILHELMINA","ROSALEE","MAURINE","LATRICE","YONG","JENA","TARYN","ELIA","DEBBY","MAUDIE","JEANNA","DELILAH","CATRINA","SHONDA","HORTENCIA","THEODORA","TERESITA","ROBBIN","DANETTE","MARYJANE","FREDDIE","DELPHINE","BRIANNE","NILDA","DANNA","CINDI","BESS","IONA","HANNA","ARIEL","WINONA","VIDA","ROSITA","MARIANNA","WILLIAM","RACHEAL","GUILLERMINA","ELOISA","CELESTINE","CAREN","MALISSA","LONA","CHANTEL","SHELLIE","MARISELA","LEORA","AGATHA","SOLEDAD","MIGDALIA","IVETTE","CHRISTEN","ATHENA","JANEL","CHLOE","VEDA","PATTIE","TESSIE","TERA","MARILYNN","LUCRETIA","KARRIE","DINAH","DANIELA","ALECIA","ADELINA","VERNICE","SHIELA","PORTIA","MERRY","LASHAWN","DEVON","DARA","TAWANA","OMA","VERDA","CHRISTIN","ALENE","ZELLA","SANDI","RAFAELA","MAYA","KIRA","CANDIDA","ALVINA","SUZAN","SHAYLA","LYN","LETTIE","ALVA","SAMATHA","ORALIA","MATILDE","MADONNA","LARISSA","VESTA","RENITA","INDIA","DELOIS","SHANDA","PHILLIS","LORRI","ERLINDA","CRUZ","CATHRINE","BARB","ZOE","ISABELL","IONE","GISELA","CHARLIE","VALENCIA","ROXANNA","MAYME","KISHA","ELLIE","MELLISSA","DORRIS","DALIA","BELLA","ANNETTA","ZOILA","RETA","REINA","LAURETTA","KYLIE","CHRISTAL","PILAR","CHARLA","ELISSA","TIFFANI","TANA","PAULINA","LEOTA","BREANNA","JAYME","CARMEL","VERNELL","TOMASA","MANDI","DOMINGA","SANTA","MELODIE","LURA","ALEXA","TAMELA","RYAN","MIRNA","KERRIE","VENUS","NOEL","FELICITA","CRISTY","CARMELITA","BERNIECE","ANNEMARIE","TIARA","ROSEANNE","MISSY","CORI","ROXANA","PRICILLA","KRISTAL","JUNG","ELYSE","HAYDEE","ALETHA","BETTINA","MARGE","GILLIAN","FILOMENA","CHARLES","ZENAIDA","HARRIETTE","CARIDAD","VADA","UNA","ARETHA","PEARLINE","MARJORY","MARCELA","FLOR","EVETTE","ELOUISE","ALINA","TRINIDAD","DAVID","DAMARIS","CATHARINE","CARROLL","BELVA","NAKIA","MARLENA","LUANNE","LORINE","KARON","DORENE","DANITA","BRENNA","TATIANA","SAMMIE","LOUANN","LOREN","JULIANNA","ANDRIA","PHILOMENA","LUCILA","LEONORA","DOVIE","ROMONA","MIMI","JACQUELIN","GAYE","TONJA","MISTI","JOE","GENE","CHASTITY","STACIA","ROXANN","MICAELA","NIKITA","MEI","VELDA","MARLYS","JOHNNA","AURA","LAVERN","IVONNE","HAYLEY","NICKI","MAJORIE","HERLINDA","GEORGE","ALPHA","YADIRA","PERLA","GREGORIA","DANIEL","ANTONETTE","SHELLI","MOZELLE","MARIAH","JOELLE","CORDELIA","JOSETTE","CHIQUITA","TRISTA","LOUIS","LAQUITA","GEORGIANA","CANDI","SHANON","LONNIE","HILDEGARD","CECIL","VALENTINA","STEPHANY","MAGDA","KAROL","GERRY","GABRIELLA","TIANA","ROMA","RICHELLE","RAY","PRINCESS","OLETA","JACQUE","IDELLA","ALAINA","SUZANNA","JOVITA","BLAIR","TOSHA","RAVEN","NEREIDA","MARLYN","KYLA","JOSEPH","DELFINA","TENA","STEPHENIE","SABINA","NATHALIE","MARCELLE","GERTIE","DARLEEN","THEA","SHARONDA","SHANTEL","BELEN","VENESSA","ROSALINA","ONA","GENOVEVA","COREY","CLEMENTINE","ROSALBA","RENATE","RENATA","MI","IVORY","GEORGIANNA","FLOY","DORCAS","ARIANA","TYRA","THEDA","MARIAM","JULI","JESICA","DONNIE","VIKKI","VERLA","ROSELYN","MELVINA","JANNETTE","GINNY","DEBRAH","CORRIE","ASIA","VIOLETA","MYRTIS","LATRICIA","COLLETTE","CHARLEEN","ANISSA","VIVIANA","TWYLA","PRECIOUS","NEDRA","LATONIA","LAN","HELLEN","FABIOLA","ANNAMARIE","ADELL","SHARYN","CHANTAL","NIKI","MAUD","LIZETTE","LINDY","KIA","KESHA","JEANA","DANELLE","CHARLINE","CHANEL","CARROL","VALORIE","LIA","DORTHA","CRISTAL","SUNNY","LEONE","LEILANI","GERRI","DEBI","ANDRA","KESHIA","IMA","EULALIA","EASTER","DULCE","NATIVIDAD","LINNIE","KAMI","GEORGIE","CATINA","BROOK","ALDA","WINNIFRED","SHARLA","RUTHANN","MEAGHAN","MAGDALENE","LISSETTE","ADELAIDA","VENITA","TRENA","SHIRLENE","SHAMEKA","ELIZEBETH","DIAN","SHANTA","MICKEY","LATOSHA","CARLOTTA","WINDY","SOON","ROSINA","MARIANN","LEISA","JONNIE","DAWNA","CATHIE","BILLY","ASTRID","SIDNEY","LAUREEN","JANEEN","HOLLI","FAWN","VICKEY","TERESSA","SHANTE","RUBYE","MARCELINA","CHANDA","CARY","TERESE","SCARLETT","MARTY","MARNIE","LULU","LISETTE","JENIFFER","ELENOR","DORINDA","DONITA","CARMAN","BERNITA","ALTAGRACIA","ALETA","ADRIANNA","ZORAIDA","RONNIE","NICOLA","LYNDSEY","KENDALL","JANINA","CHRISSY","AMI","STARLA","PHYLIS","PHUONG","KYRA","CHARISSE","BLANCH","SANJUANITA","RONA","NANCI","MARILEE","MARANDA","CORY","BRIGETTE","SANJUANA","MARITA","KASSANDRA","JOYCELYN","IRA","FELIPA","CHELSIE","BONNY","MIREYA","LORENZA","KYONG","ILEANA","CANDELARIA","TONY","TOBY","SHERIE","OK","MARK","LUCIE","LEATRICE","LAKESHIA","GERDA","EDIE","BAMBI","MARYLIN","LAVON","HORTENSE","GARNET","EVIE","TRESSA","SHAYNA","LAVINA","KYUNG","JEANETTA","SHERRILL","SHARA","PHYLISS","MITTIE","ANABEL","ALESIA","THUY","TAWANDA","RICHARD","JOANIE","TIFFANIE","LASHANDA","KARISSA","ENRIQUETA","DARIA","DANIELLA","CORINNA","ALANNA","ABBEY","ROXANE","ROSEANNA","MAGNOLIA","LIDA","KYLE","JOELLEN","ERA","CORAL","CARLEEN","TRESA","PEGGIE","NOVELLA","NILA","MAYBELLE","JENELLE","CARINA","NOVA","MELINA","MARQUERITE","MARGARETTE","JOSEPHINA","EVONNE","DEVIN","CINTHIA","ALBINA","TOYA","TAWNYA","SHERITA","SANTOS","MYRIAM","LIZABETH","LISE","KEELY","JENNI","GISELLE","CHERYLE","ARDITH","ARDIS","ALESHA","ADRIANE","SHAINA","LINNEA","KAROLYN","HONG","FLORIDA","FELISHA","DORI","DARCI","ARTIE","ARMIDA","ZOLA","XIOMARA","VERGIE","SHAMIKA","NENA","NANNETTE","MAXIE","LOVIE","JEANE","JAIMIE","INGE","FARRAH","ELAINA","CAITLYN","STARR","FELICITAS","CHERLY","CARYL","YOLONDA","YASMIN","TEENA","PRUDENCE","PENNIE","NYDIA","MACKENZIE","ORPHA","MARVEL","LIZBETH","LAURETTE","JERRIE","HERMELINDA","CAROLEE","TIERRA","MIRIAN","META","MELONY","KORI","JENNETTE","JAMILA","ENA","ANH","YOSHIKO","SUSANNAH","SALINA","RHIANNON","JOLEEN","CRISTINE","ASHTON","ARACELY","TOMEKA","SHALONDA","MARTI","LACIE","KALA","JADA","ILSE","HAILEY","BRITTANI","ZONA","SYBLE","SHERRYL","RANDY","NIDIA","MARLO","KANDICE","KANDI","DEB","DEAN","AMERICA","ALYCIA","TOMMY","RONNA","NORENE","MERCY","JOSE","INGEBORG","GIOVANNA","GEMMA","CHRISTEL","AUDRY","ZORA","VITA","VAN","TRISH","STEPHAINE","SHIRLEE","SHANIKA","MELONIE","MAZIE","JAZMIN","INGA","HOA","HETTIE","GERALYN","FONDA","ESTRELLA","ADELLA","SU","SARITA","RINA","MILISSA","MARIBETH","GOLDA","EVON","ETHELYN","ENEDINA","CHERISE","CHANA","VELVA","TAWANNA","SADE","MIRTA","LI","KARIE","JACINTA","ELNA","DAVINA","CIERRA","ASHLIE","ALBERTHA","TANESHA","STEPHANI","NELLE","MINDI","LU","LORINDA","LARUE","FLORENE","DEMETRA","DEDRA","CIARA","CHANTELLE","ASHLY","SUZY","ROSALVA","NOELIA","LYDA","LEATHA","KRYSTYNA","KRISTAN","KARRI","DARLINE","DARCIE","CINDA","CHEYENNE","CHERRIE","AWILDA","ALMEDA","ROLANDA","LANETTE","JERILYN","GISELE","EVALYN","CYNDI","CLETA","CARIN","ZINA","ZENA","VELIA","TANIKA","PAUL","CHARISSA","THOMAS","TALIA","MARGARETE","LAVONDA","KAYLEE","KATHLENE","JONNA","IRENA","ILONA","IDALIA","CANDIS","CANDANCE","BRANDEE","ANITRA","ALIDA","SIGRID","NICOLETTE","MARYJO","LINETTE","HEDWIG","CHRISTIANA","CASSIDY","ALEXIA","TRESSIE","MODESTA","LUPITA","LITA","GLADIS","EVELIA","DAVIDA","CHERRI","CECILY","ASHELY","ANNABEL","AGUSTINA","WANITA","SHIRLY","ROSAURA","HULDA","EUN","BAILEY","YETTA","VERONA","THOMASINA","SIBYL","SHANNAN","MECHELLE","LUE","LEANDRA","LANI","KYLEE","KANDY","JOLYNN","FERNE","EBONI","CORENE","ALYSIA","ZULA","NADA","MOIRA","LYNDSAY","LORRETTA","JUAN","JAMMIE","HORTENSIA","GAYNELL","CAMERON","ADRIA","VINA","VICENTA","TANGELA","STEPHINE","NORINE","NELLA","LIANA","LESLEE","KIMBERELY","ILIANA","GLORY","FELICA","EMOGENE","ELFRIEDE","EDEN","EARTHA","CARMA","BEA","OCIE","MARRY","LENNIE","KIARA","JACALYN","CARLOTA","ARIELLE","YU","STAR","OTILIA","KIRSTIN","KACEY","JOHNETTA","JOEY","JOETTA","JERALDINE","JAUNITA","ELANA","DORTHEA","CAMI","AMADA","ADELIA","VERNITA","TAMAR","SIOBHAN","RENEA","RASHIDA","OUIDA","ODELL","NILSA","MERYL","KRISTYN","JULIETA","DANICA","BREANNE","AUREA","ANGLEA","SHERRON","ODETTE","MALIA","LORELEI","LIN","LEESA","KENNA","KATHLYN","FIONA","CHARLETTE","SUZIE","SHANTELL","SABRA","RACQUEL","MYONG","MIRA","MARTINE","LUCIENNE","LAVADA","JULIANN","JOHNIE","ELVERA","DELPHIA","CLAIR","CHRISTIANE","CHAROLETTE","CARRI","AUGUSTINE","ASHA","ANGELLA","PAOLA","NINFA","LEDA","LAI","EDA","SUNSHINE","STEFANI","SHANELL","PALMA","MACHELLE","LISSA","KECIA","KATHRYNE","KARLENE","JULISSA","JETTIE","JENNIFFER","HUI","CORRINA","CHRISTOPHER","CAROLANN","ALENA","TESS","ROSARIA","MYRTICE","MARYLEE","LIANE","KENYATTA","JUDIE","JANEY","IN","ELMIRA","ELDORA","DENNA","CRISTI","CATHI","ZAIDA","VONNIE","VIVA","VERNIE","ROSALINE","MARIELA","LUCIANA","LESLI","KARAN","FELICE","DENEEN","ADINA","WYNONA","TARSHA","SHERON","SHASTA","SHANITA","SHANI","SHANDRA","RANDA","PINKIE","PARIS","NELIDA","MARILOU","LYLA","LAURENE","LACI","JOI","JANENE","DOROTHA","DANIELE","DANI","CAROLYNN","CARLYN","BERENICE","AYESHA","ANNELIESE","ALETHEA","THERSA","TAMIKO","RUFINA","OLIVA","MOZELL","MARYLYN","MADISON","KRISTIAN","KATHYRN","KASANDRA","KANDACE","JANAE","GABRIEL","DOMENICA","DEBBRA","DANNIELLE","CHUN","BUFFY","BARBIE","ARCELIA","AJA","ZENOBIA","SHAREN","SHAREE","PATRICK","PAGE","MY","LAVINIA","KUM","KACIE","JACKELINE","HUONG","FELISA","EMELIA","ELEANORA","CYTHIA","CRISTIN","CLYDE","CLARIBEL","CARON","ANASTACIA","ZULMA","ZANDRA","YOKO","TENISHA","SUSANN","SHERILYN","SHAY","SHAWANDA","SABINE","ROMANA","MATHILDA","LINSEY","KEIKO","JOANA","ISELA","GRETTA","GEORGETTA","EUGENIE","DUSTY","DESIRAE","DELORA","CORAZON","ANTONINA","ANIKA","WILLENE","TRACEE","TAMATHA","REGAN","NICHELLE","MICKIE","MAEGAN","LUANA","LANITA","KELSIE","EDELMIRA","BREE","AFTON","TEODORA","TAMIE","SHENA","MEG","LINH","KELI","KACI","DANYELLE","BRITT","ARLETTE","ALBERTINE","ADELLE","TIFFINY","STORMY","SIMONA","NUMBERS","NICOLASA","NICHOL","NIA","NAKISHA","MEE","MAIRA","LOREEN","KIZZY","JOHNNY","JAY","FALLON","CHRISTENE","BOBBYE","ANTHONY","YING","VINCENZA","TANJA","RUBIE","RONI","QUEENIE","MARGARETT","KIMBERLI","IRMGARD","IDELL","HILMA","EVELINA","ESTA","EMILEE","DENNISE","DANIA","CARL","CARIE","ANTONIO","WAI","SANG","RISA","RIKKI","PARTICIA","MUI","MASAKO","MARIO","LUVENIA","LOREE","LONI","LIEN","KEVIN","GIGI","FLORENCIA","DORIAN","DENITA","DALLAS","CHI","BILLYE","ALEXANDER","TOMIKA","SHARITA","RANA","NIKOLE","NEOMA","MARGARITE","MADALYN","LUCINA","LAILA","KALI","JENETTE","GABRIELE","EVELYNE","ELENORA","CLEMENTINA","ALEJANDRINA","ZULEMA","VIOLETTE","VANNESSA","THRESA","RETTA","PIA","PATIENCE","NOELLA","NICKIE","JONELL","DELTA","CHUNG","CHAYA","CAMELIA","BETHEL","ANYA","ANDREW","THANH","SUZANN","SPRING","SHU","MILA","LILLA","LAVERNA","KEESHA","KATTIE","GIA","GEORGENE","EVELINE","ESTELL","ELIZBETH","VIVIENNE","VALLIE","TRUDIE","STEPHANE","MICHEL","MAGALY","MADIE","KENYETTA","KARREN","JANETTA","HERMINE","HARMONY","DRUCILLA","DEBBI","CELESTINA","CANDIE","BRITNI","BECKIE","AMINA","ZITA","YUN","YOLANDE","VIVIEN","VERNETTA","TRUDI","SOMMER","PEARLE","PATRINA","OSSIE","NICOLLE","LOYCE","LETTY","LARISA","KATHARINA","JOSELYN","JONELLE","JENELL","IESHA","HEIDE","FLORINDA","FLORENTINA","FLO","ELODIA","DORINE","BRUNILDA","BRIGID","ASHLI","ARDELLA","TWANA","THU","TARAH","SUNG","SHEA","SHAVON","SHANE","SERINA","RAYNA","RAMONITA","NGA","MARGURITE","LUCRECIA","KOURTNEY","KATI","JESUS","JESENIA","DIAMOND","CRISTA","AYANA","ALICA","ALIA","VINNIE","SUELLEN","ROMELIA","RACHELL","PIPER","OLYMPIA","MICHIKO","KATHALEEN","JOLIE","JESSI","JANESSA","HANA","HA","ELEASE","CARLETTA","BRITANY","SHONA","SALOME","ROSAMOND","REGENA","RAINA","NGOC","NELIA","LOUVENIA","LESIA","LATRINA","LATICIA","LARHONDA","JINA","JACKI","HOLLIS","HOLLEY","EMMY","DEEANN","CORETTA","ARNETTA","VELVET","THALIA","SHANICE","NETA","MIKKI","MICKI","LONNA","LEANA","LASHUNDA","KILEY","JOYE","JACQULYN","IGNACIA","HYUN","HIROKO","HENRY","HENRIETTE","ELAYNE","DELINDA","DARNELL","DAHLIA","COREEN","CONSUELA","CONCHITA","CELINE","BABETTE","AYANNA","ANETTE","ALBERTINA","SKYE","SHAWNEE","SHANEKA","QUIANA","PAMELIA","MIN","MERRI","MERLENE","MARGIT","KIESHA","KIERA","KAYLENE","JODEE","JENISE","ERLENE","EMMIE","ELSE","DARYL","DALILA","DAISEY","CODY","CASIE","BELIA","BABARA","VERSIE","VANESA","SHELBA","SHAWNDA","SAM","NORMAN","NIKIA","NAOMA","MARNA","MARGERET","MADALINE","LAWANA","KINDRA","JUTTA","JAZMINE","JANETT","HANNELORE","GLENDORA","GERTRUD","GARNETT","FREEDA","FREDERICA","FLORANCE","FLAVIA","DENNIS","CARLINE","BEVERLEE","ANJANETTE","VALDA","TRINITY","TAMALA","STEVIE","SHONNA","SHA","SARINA","ONEIDA","MICAH","MERILYN","MARLEEN","LURLINE","LENNA","KATHERIN","JIN","JENI","HAE","GRACIA","GLADY","FARAH","ERIC","ENOLA","EMA","DOMINQUE","DEVONA","DELANA","CECILA","CAPRICE","ALYSHA","ALI","ALETHIA","VENA","THERESIA","TAWNY","SONG","SHAKIRA","SAMARA","SACHIKO","RACHELE","PAMELLA","NICKY","MARNI","MARIEL","MAREN","MALISA","LIGIA","LERA","LATORIA","LARAE","KIMBER","KATHERN","KAREY","JENNEFER","JANETH","HALINA","FREDIA","DELISA","DEBROAH","CIERA","CHIN","ANGELIKA","ANDREE","ALTHA","YEN","VIVAN","TERRESA","TANNA","SUK","SUDIE","SOO","SIGNE","SALENA","RONNI","REBBECCA","MYRTIE","MCKENZIE","MALIKA","MAIDA","LOAN","LEONARDA","KAYLEIGH","FRANCE","ETHYL","ELLYN","DAYLE","CAMMIE","BRITTNI","BIRGIT","AVELINA","ASUNCION","ARIANNA","AKIKO","VENICE","TYESHA","TONIE","TIESHA","TAKISHA","STEFFANIE","SINDY","SANTANA","MEGHANN","MANDA","MACIE","LADY","KELLYE","KELLEE","JOSLYN","JASON","INGER","INDIRA","GLINDA","GLENNIS","FERNANDA","FAUSTINA","ENEIDA","ELICIA","DOT","DIGNA","DELL","ARLETTA","ANDRE","WILLIA","TAMMARA","TABETHA","SHERRELL","SARI","REFUGIO","REBBECA","PAULETTA","NIEVES","NATOSHA","NAKITA","MAMMIE","KENISHA","KAZUKO","KASSIE","GARY","EARLEAN","DAPHINE","CORLISS","CLOTILDE","CAROLYNE","BERNETTA","AUGUSTINA","AUDREA","ANNIS","ANNABELL","YAN","TENNILLE","TAMICA","SELENE","SEAN","ROSANA","REGENIA","QIANA","MARKITA","MACY","LEEANNE","LAURINE","KYM","JESSENIA","JANITA","GEORGINE","GENIE","EMIKO","ELVIE","DEANDRA","DAGMAR","CORIE","COLLEN","CHERISH","ROMAINE","PORSHA","PEARLENE","MICHELINE","MERNA","MARGORIE","MARGARETTA","LORE","KENNETH","JENINE","HERMINA","FREDERICKA","ELKE","DRUSILLA","DORATHY","DIONE","DESIRE","CELENA","BRIGIDA","ANGELES","ALLEGRA","THEO","TAMEKIA","SYNTHIA","STEPHEN","SOOK","SLYVIA","ROSANN","REATHA","RAYE","MARQUETTA","MARGART","LING","LAYLA","KYMBERLY","KIANA","KAYLEEN","KATLYN","KARMEN","JOELLA","IRINA","EMELDA","ELENI","DETRA","CLEMMIE","CHERYLL","CHANTELL","CATHEY","ARNITA","ARLA","ANGLE","ANGELIC","ALYSE","ZOFIA","THOMASINE","TENNIE","SON","SHERLY","SHERLEY","SHARYL","REMEDIOS","PETRINA","NICKOLE","MYUNG","MYRLE","MOZELLA","LOUANNE","LISHA","LATIA","LANE","KRYSTA","JULIENNE","JOEL","JEANENE","JACQUALINE","ISAURA","GWENDA","EARLEEN","DONALD","CLEOPATRA","CARLIE","AUDIE","ANTONIETTA","ALISE","ALEX","VERDELL","VAL","TYLER","TOMOKO","THAO","TALISHA","STEVEN","SO","SHEMIKA","SHAUN","SCARLET","SAVANNA","SANTINA","ROSIA","RAEANN","ODILIA","NANA","MINNA","MAGAN","LYNELLE","LE","KARMA","JOEANN","IVANA","INELL","ILANA","HYE","HONEY","HEE","GUDRUN","FRANK","DREAMA","CRISSY","CHANTE","CARMELINA","ARVILLA","ARTHUR","ANNAMAE","ALVERA","ALEIDA","AARON","YEE","YANIRA","VANDA","TIANNA","TAM","STEFANIA","SHIRA","PERRY","NICOL","NANCIE","MONSERRATE","MINH","MELYNDA","MELANY","MATTHEW","LOVELLA","LAURE","KIRBY","KACY","JACQUELYNN","HYON","GERTHA","FRANCISCO","ELIANA","CHRISTENA","CHRISTEEN","CHARISE","CATERINA","CARLEY","CANDYCE","ARLENA","AMMIE","YANG","WILLETTE","VANITA","TUYET","TINY","SYREETA","SILVA","SCOTT","RONALD","PENNEY","NYLA","MICHAL","MAURICE","MARYAM","MARYA","MAGEN","LUDIE","LOMA","LIVIA","LANELL","KIMBERLIE","JULEE","DONETTA","DIEDRA","DENISHA","DEANE","DAWNE","CLARINE","CHERRYL","BRONWYN","BRANDON","ALLA","VALERY","TONDA","SUEANN","SORAYA","SHOSHANA","SHELA","SHARLEEN","SHANELLE","NERISSA","MICHEAL","MERIDITH","MELLIE","MAYE","MAPLE","MAGARET","LUIS","LILI","LEONILA","LEONIE","LEEANNA","LAVONIA","LAVERA","KRISTEL","KATHEY","KATHE","JUSTIN","JULIAN","JIMMY","JANN","ILDA","HILDRED","HILDEGARDE","GENIA","FUMIKO","EVELIN","ERMELINDA","ELLY","DUNG","DOLORIS","DIONNA","DANAE","BERNEICE","ANNICE","ALIX","VERENA","VERDIE","TRISTAN","SHAWNNA","SHAWANA","SHAUNNA","ROZELLA","RANDEE","RANAE","MILAGRO","LYNELL","LUISE","LOUIE","LOIDA","LISBETH","KARLEEN","JUNITA","JONA","ISIS","HYACINTH","HEDY","GWENN","ETHELENE","ERLINE","EDWARD","DONYA","DOMONIQUE","DELICIA","DANNETTE","CICELY","BRANDA","BLYTHE","BETHANN","ASHLYN","ANNALEE","ALLINE","YUKO","VELLA","TRANG","TOWANDA","TESHA","SHERLYN","NARCISA","MIGUELINA","MERI","MAYBELL","MARLANA","MARGUERITA","MADLYN","LUNA","LORY","LORIANN","LIBERTY","LEONORE","LEIGHANN","LAURICE","LATESHA","LARONDA","KATRICE","KASIE","KARL","KALEY","JADWIGA","GLENNIE","GEARLDINE","FRANCINA","EPIFANIA","DYAN","DORIE","DIEDRE","DENESE","DEMETRICE","DELENA","DARBY","CRISTIE","CLEORA","CATARINA","CARISA","BERNIE","BARBERA","ALMETA","TRULA","TEREASA","SOLANGE","SHEILAH","SHAVONNE","SANORA","ROCHELL","MATHILDE","MARGARETA","MAIA","LYNSEY","LAWANNA","LAUNA","KENA","KEENA","KATIA","JAMEY","GLYNDA","GAYLENE","ELVINA","ELANOR","DANUTA","DANIKA","CRISTEN","CORDIE","COLETTA","CLARITA","CARMON","BRYNN","AZUCENA","AUNDREA","ANGELE","YI","WALTER","VERLIE","VERLENE","TAMESHA","SILVANA","SEBRINA","SAMIRA","REDA","RAYLENE","PENNI","PANDORA","NORAH","NOMA","MIREILLE","MELISSIA","MARYALICE","LARAINE","KIMBERY","KARYL","KARINE","KAM","JOLANDA","JOHANA","JESUSA","JALEESA","JAE","JACQUELYNE","IRISH","ILUMINADA","HILARIA","HANH","GENNIE","FRANCIE","FLORETTA","EXIE","EDDA","DREMA","DELPHA","BEV","BARBAR","ASSUNTA","ARDELL","ANNALISA","ALISIA","YUKIKO","YOLANDO","WONDA","WEI","WALTRAUD","VETA","TEQUILA","TEMEKA","TAMEIKA","SHIRLEEN","SHENITA","PIEDAD","OZELLA","MIRTHA","MARILU","KIMIKO","JULIANE","JENICE","JEN","JANAY","JACQUILINE","HILDE","FE","FAE","EVAN","EUGENE","ELOIS","ECHO","DEVORAH","CHAU","BRINDA","BETSEY","ARMINDA","ARACELIS","APRYL","ANNETT","ALISHIA","VEOLA","USHA","TOSHIKO","THEOLA","TASHIA","TALITHA","SHERY","RUDY","RENETTA","REIKO","RASHEEDA","OMEGA","OBDULIA","MIKA","MELAINE","MEGGAN","MARTIN","MARLEN","MARGET","MARCELINE","MANA","MAGDALEN","LIBRADA","LEZLIE","LEXIE","LATASHIA","LASANDRA","KELLE","ISIDRA","ISA","INOCENCIA","GWYN","FRANCOISE","ERMINIA","ERINN","DIMPLE","DEVORA","CRISELDA","ARMANDA","ARIE","ARIANE","ANGELO","ANGELENA","ALLEN","ALIZA","ADRIENE","ADALINE","XOCHITL","TWANNA","TRAN","TOMIKO","TAMISHA","TAISHA","SUSY","SIU","RUTHA","ROXY","RHONA","RAYMOND","OTHA","NORIKO","NATASHIA","MERRIE","MELVIN","MARINDA","MARIKO","MARGERT","LORIS","LIZZETTE","LEISHA","KAILA","KA","JOANNIE","JERRICA","JENE","JANNET","JANEE","JACINDA","HERTA","ELENORE","DORETTA","DELAINE","DANIELL","CLAUDIE","CHINA","BRITTA","APOLONIA","AMBERLY","ALEASE","YURI","YUK","WEN","WANETA","UTE","TOMI","SHARRI","SANDIE","ROSELLE","REYNALDA","RAGUEL","PHYLICIA","PATRIA","OLIMPIA","ODELIA","MITZIE","MITCHELL","MISS","MINDA","MIGNON","MICA","MENDY","MARIVEL","MAILE","LYNETTA","LAVETTE","LAURYN","LATRISHA","LAKIESHA","KIERSTEN","KARY","JOSPHINE","JOLYN","JETTA","JANISE","JACQUIE","IVELISSE","GLYNIS","GIANNA","GAYNELLE","EMERALD","DEMETRIUS","DANYELL","DANILLE","DACIA","CORALEE","CHER","CEOLA","BRETT","BELL","ARIANNE","ALESHIA","YUNG","WILLIEMAE","TROY","TRINH","THORA","TAI","SVETLANA","SHERIKA","SHEMEKA","SHAUNDA","ROSELINE","RICKI","MELDA","MALLIE","LAVONNA","LATINA","LARRY","LAQUANDA","LALA","LACHELLE","KLARA","KANDIS","JOHNA","JEANMARIE","JAYE","HANG","GRAYCE","GERTUDE","EMERITA","EBONIE","CLORINDA","CHING","CHERY","CAROLA","BREANN","BLOSSOM","BERNARDINE","BECKI","ARLETHA","ARGELIA","ARA","ALITA","YULANDA","YON","YESSENIA","TOBI","TASIA","SYLVIE","SHIRL","SHIRELY","SHERIDAN","SHELLA","SHANTELLE","SACHA","ROYCE","REBECKA","REAGAN","PROVIDENCIA","PAULENE","MISHA","MIKI","MARLINE","MARICA","LORITA","LATOYIA","LASONYA","KERSTIN","KENDA","KEITHA","KATHRIN","JAYMIE","JACK","GRICELDA","GINETTE","ERYN","ELINA","ELFRIEDA","DANYEL","CHEREE","CHANELLE","BARRIE","AVERY","AURORE","ANNAMARIA","ALLEEN","AILENE","AIDE","YASMINE","VASHTI","VALENTINE","TREASA","TORY","TIFFANEY","SHERYLL","SHARIE","SHANAE","SAU","RAISA","PA","NEDA","MITSUKO","MIRELLA","MILDA","MARYANNA","MARAGRET","MABELLE","LUETTA","LORINA","LETISHA","LATARSHA","LANELLE","LAJUANA","KRISSY","KARLY","KARENA","JON","JESSIKA","JERICA","JEANELLE","JANUARY","JALISA","JACELYN","IZOLA","IVEY","GREGORY","EUNA","ETHA","DREW","DOMITILA","DOMINICA","DAINA","CREOLA","CARLI","CAMIE","BUNNY","BRITTNY","ASHANTI","ANISHA","ALEEN","ADAH","YASUKO","WINTER","VIKI","VALRIE","TONA","TINISHA","THI","TERISA","TATUM","TANEKA","SIMONNE","SHALANDA","SERITA","RESSIE","REFUGIA","PAZ","OLENE","NA","MERRILL","MARGHERITA","MANDIE","MAN","MAIRE","LYNDIA","LUCI","LORRIANE","LORETA","LEONIA","LAVONA","LASHAWNDA","LAKIA","KYOKO","KRYSTINA","KRYSTEN","KENIA","KELSI","JUDE","JEANICE","ISOBEL","GEORGIANN","GENNY","FELICIDAD","EILENE","DEON","DELOISE","DEEDEE","DANNIE","CONCEPTION","CLORA","CHERILYN","CHANG","CALANDRA","BERRY","ARMANDINA","ANISA","ULA","TIMOTHY","TIERA","THERESSA","STEPHANIA","SIMA","SHYLA","SHONTA","SHERA","SHAQUITA","SHALA","SAMMY","ROSSANA","NOHEMI","NERY","MORIAH","MELITA","MELIDA","MELANI","MARYLYNN","MARISHA","MARIETTE","MALORIE","MADELENE","LUDIVINA","LORIA","LORETTE","LORALEE","LIANNE","LEON","LAVENIA","LAURINDA","LASHON","KIT","KIMI","KEILA","KATELYNN","KAI","JONE","JOANE","JI","JAYNA","JANELLA","JA","HUE","HERTHA","FRANCENE","ELINORE","DESPINA","DELSIE","DEEDRA","CLEMENCIA","CARRY","CAROLIN","CARLOS","BULAH","BRITTANIE","BOK","BLONDELL","BIBI","BEAULAH","BEATA","ANNITA","AGRIPINA","VIRGEN","VALENE","UN","TWANDA","TOMMYE","TOI","TARRA","TARI","TAMMERA","SHAKIA","SADYE","RUTHANNE","ROCHEL","RIVKA","PURA","NENITA","NATISHA","MING","MERRILEE","MELODEE","MARVIS","LUCILLA","LEENA","LAVETA","LARITA","LANIE","KEREN","ILEEN","GEORGEANN","GENNA","GENESIS","FRIDA","EWA","EUFEMIA","EMELY","ELA","EDYTH","DEONNA","DEADRA","DARLENA","CHANELL","CHAN","CATHERN","CASSONDRA","CASSAUNDRA","BERNARDA","BERNA","ARLINDA","ANAMARIA","ALBERT","WESLEY","VERTIE","VALERI","TORRI","TATYANA","STASIA","SHERISE","SHERILL","SEASON","SCOTTIE","SANDA","RUTHE","ROSY","ROBERTO","ROBBI","RANEE","QUYEN","PEARLY","PALMIRA","ONITA","NISHA","NIESHA","NIDA","NEVADA","NAM","MERLYN","MAYOLA","MARYLOUISE","MARYLAND","MARX","MARTH","MARGENE","MADELAINE","LONDA","LEONTINE","LEOMA","LEIA","LAWRENCE","LAURALEE","LANORA","LAKITA","KIYOKO","KETURAH","KATELIN","KAREEN","JONIE","JOHNETTE","JENEE","JEANETT","IZETTA","HIEDI","HEIKE","HASSIE","HAROLD","GIUSEPPINA","GEORGANN","FIDELA","FERNANDE","ELWANDA","ELLAMAE","ELIZ","DUSTI","DOTTY","CYNDY","CORALIE","CELESTA","ARGENTINA","ALVERTA","XENIA","WAVA","VANETTA","TORRIE","TASHINA","TANDY","TAMBRA","TAMA","STEPANIE","SHILA","SHAUNTA","SHARAN","SHANIQUA","SHAE","SETSUKO","SERAFINA","SANDEE","ROSAMARIA","PRISCILA","OLINDA","NADENE","MUOI","MICHELINA","MERCEDEZ","MARYROSE","MARIN","MARCENE","MAO","MAGALI","MAFALDA","LOGAN","LINN","LANNIE","KAYCE","KAROLINE","KAMILAH","KAMALA","JUSTA","JOLINE","JENNINE","JACQUETTA","IRAIDA","GERALD","GEORGEANNA","FRANCHESCA","FAIRY","EMELINE","ELANE","EHTEL","EARLIE","DULCIE","DALENE","CRIS","CLASSIE","CHERE","CHARIS","CAROYLN","CARMINA","CARITA","BRIAN","BETHANIE","AYAKO","ARICA","AN","ALYSA","ALESSANDRA","AKILAH","ADRIEN","ZETTA","YOULANDA","YELENA","YAHAIRA","XUAN","WENDOLYN","VICTOR","TIJUANA","TERRELL","TERINA","TERESIA","SUZI","SUNDAY","SHERELL","SHAVONDA","SHAUNTE","SHARDA","SHAKITA","SENA","RYANN","RUBI","RIVA","REGINIA","REA","RACHAL","PARTHENIA","PAMULA","MONNIE","MONET","MICHAELE","MELIA","MARINE","MALKA","MAISHA","LISANDRA","LEO","LEKISHA","LEAN","LAURENCE","LAKENDRA","KRYSTIN","KORTNEY","KIZZIE","KITTIE","KERA","KENDAL","KEMBERLY","KANISHA","JULENE","JULE","JOSHUA","JOHANNE","JEFFREY","JAMEE","HAN","HALLEY","GIDGET","GALINA","FREDRICKA","FLETA","FATIMAH","EUSEBIA","ELZA","ELEONORE","DORTHEY","DORIA","DONELLA","DINORAH","DELORSE","CLARETHA","CHRISTINIA","CHARLYN","BONG","BELKIS","AZZIE","ANDERA","AIKO","ADENA","YER","YAJAIRA","WAN","VANIA","ULRIKE","TOSHIA","TIFANY","STEFANY","SHIZUE","SHENIKA","SHAWANNA","SHAROLYN","SHARILYN","SHAQUANA","SHANTAY","SEE","ROZANNE","ROSELEE","RICKIE","REMONA","REANNA","RAELENE","QUINN","PHUNG","PETRONILA","NATACHA","NANCEY","MYRL","MIYOKO","MIESHA","MERIDETH","MARVELLA","MARQUITTA","MARHTA","MARCHELLE","LIZETH","LIBBIE","LAHOMA","LADAWN","KINA","KATHELEEN","KATHARYN","KARISA","KALEIGH","JUNIE","JULIEANN","JOHNSIE","JANEAN","JAIMEE","JACKQUELINE","HISAKO","HERMA","HELAINE","GWYNETH","GLENN","GITA","EUSTOLIA","EMELINA","ELIN","EDRIS","DONNETTE","DONNETTA","DIERDRE","DENAE","DARCEL","CLAUDE","CLARISA","CINDERELLA","CHIA","CHARLESETTA","CHARITA","CELSA","CASSY","CASSI","CARLEE","BRUNA","BRITTANEY","BRANDE","BILLI","BAO","ANTONETTA","ANGLA","ANGELYN","ANALISA","ALANE","WENONA","WENDIE","VERONIQUE","VANNESA","TOBIE","TEMPIE","SUMIKO","SULEMA","SPARKLE","SOMER","SHEBA","SHAYNE","SHARICE","SHANEL","SHALON","SAGE","ROY","ROSIO","ROSELIA","RENAY","REMA","REENA","PORSCHE","PING","PEG","OZIE","ORETHA","ORALEE","ODA","NU","NGAN","NAKESHA","MILLY","MARYBELLE","MARLIN","MARIS","MARGRETT","MARAGARET","MANIE","LURLENE","LILLIA","LIESELOTTE","LAVELLE","LASHAUNDA","LAKEESHA","KEITH","KAYCEE","KALYN","JOYA","JOETTE","JENAE","JANIECE","ILLA","GRISEL","GLAYDS","GENEVIE","GALA","FREDDA","FRED","ELMER","ELEONOR","DEBERA","DEANDREA","DAN","CORRINNE","CORDIA","CONTESSA","COLENE","CLEOTILDE","CHARLOTT","CHANTAY","CECILLE","BEATRIS","AZALEE","ARLEAN","ARDATH","ANJELICA","ANJA","ALFREDIA","ALEISHA","ADAM","ZADA","YUONNE","XIAO","WILLODEAN","WHITLEY","VENNIE","VANNA","TYISHA","TOVA","TORIE","TONISHA","TILDA","TIEN","TEMPLE","SIRENA","SHERRIL","SHANTI","SHAN","SENAIDA","SAMELLA","ROBBYN","RENDA","REITA","PHEBE","PAULITA","NOBUKO","NGUYET","NEOMI","MOON","MIKAELA","MELANIA","MAXIMINA","MARG","MAISIE","LYNNA","LILLI","LAYNE","LASHAUN","LAKENYA","LAEL","KIRSTIE","KATHLINE","KASHA","KARLYN","KARIMA","JOVAN","JOSEFINE","JENNELL","JACQUI","JACKELYN","HYO","HIEN","GRAZYNA","FLORRIE","FLORIA","ELEONORA","DWANA","DORLA","DONG","DELMY","DEJA","DEDE","DANN","CRYSTA","CLELIA","CLARIS","CLARENCE","CHIEKO","CHERLYN","CHERELLE","CHARMAIN","CHARA","CAMMY","BEE","ARNETTE","ARDELLE","ANNIKA","AMIEE","AMEE","ALLENA","YVONE","YUKI","YOSHIE","YEVETTE","YAEL","WILLETTA","VONCILE","VENETTA","TULA","TONETTE","TIMIKA","TEMIKA","TELMA","TEISHA","TAREN","TA","STACEE","SHIN","SHAWNTA","SATURNINA","RICARDA","POK","PASTY","ONIE","NUBIA","MORA","MIKE","MARIELLE","MARIELLA","MARIANELA","MARDELL","MANY","LUANNA","LOISE","LISABETH","LINDSY","LILLIANA","LILLIAM","LELAH","LEIGHA","LEANORA","LANG","KRISTEEN","KHALILAH","KEELEY","KANDRA","JUNKO","JOAQUINA","JERLENE","JANI","JAMIKA","JAME","HSIU","HERMILA","GOLDEN","GENEVIVE","EVIA","EUGENA","EMMALINE","ELFREDA","ELENE","DONETTE","DELCIE","DEEANNA","DARCEY","CUC","CLARINDA","CIRA","CHAE","CELINDA","CATHERYN","CATHERIN","CASIMIRA","CARMELIA","CAMELLIA","BREANA","BOBETTE","BERNARDINA","BEBE","BASILIA","ARLYNE","AMAL","ALAYNA","ZONIA","ZENIA","YURIKO","YAEKO","WYNELL","WILLOW","WILLENA","VERNIA","TU","TRAVIS","TORA","TERRILYN","TERICA","TENESHA","TAWNA","TAJUANA","TAINA","STEPHNIE","SONA","SOL","SINA","SHONDRA","SHIZUKO","SHERLENE","SHERICE","SHARIKA","ROSSIE","ROSENA","RORY","RIMA","RIA","RHEBA","RENNA","PETER","NATALYA","NANCEE","MELODI","MEDA","MAXIMA","MATHA","MARKETTA","MARICRUZ","MARCELENE","MALVINA","LUBA","LOUETTA","LEIDA","LECIA","LAURAN","LASHAWNA","LAINE","KHADIJAH","KATERINE","KASI","KALLIE","JULIETTA","JESUSITA","JESTINE","JESSIA","JEREMY","JEFFIE","JANYCE","ISADORA","GEORGIANNE","FIDELIA","EVITA","EURA","EULAH","ESTEFANA","ELSY","ELIZABET","ELADIA","DODIE","DION","DIA","DENISSE","DELORAS","DELILA","DAYSI","DAKOTA","CURTIS","CRYSTLE","CONCHA","COLBY","CLARETTA","CHU","CHRISTIA","CHARLSIE","CHARLENA","CARYLON","BETTYANN","ASLEY","ASHLEA","AMIRA","AI","AGUEDA","AGNUS","YUETTE","VINITA","VICTORINA","TYNISHA","TREENA","TOCCARA","TISH","THOMASENA","TEGAN","SOILA","SHILOH","SHENNA","SHARMAINE","SHANTAE","SHANDI","SEPTEMBER","SARAN","SARAI","SANA","SAMUEL","SALLEY","ROSETTE","ROLANDE","REGINE","OTELIA","OSCAR","OLEVIA","NICHOLLE","NECOLE","NAIDA","MYRTA","MYESHA","MITSUE","MINTA","MERTIE","MARGY","MAHALIA","MADALENE","LOVE","LOURA","LOREAN","LEWIS","LESHA","LEONIDA","LENITA","LAVONE","LASHELL","LASHANDRA","LAMONICA","KIMBRA","KATHERINA","KARRY","KANESHA","JULIO","JONG","JENEVA","JAQUELYN","HWA","GILMA","GHISLAINE","GERTRUDIS","FRANSISCA","FERMINA","ETTIE","ETSUKO","ELLIS","ELLAN","ELIDIA","EDRA","DORETHEA","DOREATHA","DENYSE","DENNY","DEETTA","DAINE","CYRSTAL","CORRIN","CAYLA","CARLITA","CAMILA","BURMA","BULA","BUENA","BLAKE","BARABARA","AVRIL","AUSTIN","ALAINE","ZANA","WILHEMINA","WANETTA","VIRGIL","VI","VERONIKA","VERNON","VERLINE","VASILIKI","TONITA","TISA","TEOFILA","TAYNA","TAUNYA","TANDRA","TAKAKO","SUNNI","SUANNE","SIXTA","SHARELL","SEEMA","RUSSELL","ROSENDA","ROBENA","RAYMONDE","PEI","PAMILA","OZELL","NEIDA","NEELY","MISTIE","MICHA","MERISSA","MAURITA","MARYLN","MARYETTA","MARSHALL","MARCELL","MALENA","MAKEDA","MADDIE","LOVETTA","LOURIE","LORRINE","LORILEE","LESTER","LAURENA","LASHAY","LARRAINE","LAREE","LACRESHA","KRISTLE","KRISHNA","KEVA","KEIRA","KAROLE","JOIE","JINNY","JEANNETTA","JAMA","HEIDY","GILBERTE","GEMA","FAVIOLA","EVELYNN","ENDA","ELLI","ELLENA","DIVINA","DAGNY","COLLENE","CODI","CINDIE","CHASSIDY","CHASIDY","CATRICE","CATHERINA","CASSEY","CAROLL","CARLENA","CANDRA","CALISTA","BRYANNA","BRITTENY","BEULA","BARI","AUDRIE","AUDRIA","ARDELIA","ANNELLE","ANGILA","ALONA","ALLYN","DOUGLAS","ROGER","JONATHAN","RALPH","NICHOLAS","BENJAMIN","BRUCE","HARRY","WAYNE","STEVE","HOWARD","ERNEST","PHILLIP","TODD","CRAIG","ALAN","PHILIP","EARL","DANNY","BRYAN","STANLEY","LEONARD","NATHAN","MANUEL","RODNEY","MARVIN","VINCENT","JEFFERY","JEFF","CHAD","JACOB","ALFRED","BRADLEY","HERBERT","FREDERICK","EDWIN","DON","RICKY","RANDALL","BARRY","BERNARD","LEROY","MARCUS","THEODORE","CLIFFORD","MIGUEL","JIM","TOM","CALVIN","BILL","LLOYD","DEREK","WARREN","DARRELL","JEROME","FLOYD","ALVIN","TIM","GORDON","GREG","JORGE","DUSTIN","PEDRO","DERRICK","ZACHARY","HERMAN","GLEN","HECTOR","RICARDO","RICK","BRENT","RAMON","GILBERT","MARC","REGINALD","RUBEN","NATHANIEL","RAFAEL","EDGAR","MILTON","RAUL","BEN","CHESTER","DUANE","FRANKLIN","BRAD","RON","ROLAND","ARNOLD","HARVEY","JARED","ERIK","DARRYL","NEIL","JAVIER","FERNANDO","CLINTON","TED","MATHEW","TYRONE","DARREN","LANCE","KURT","ALLAN","NELSON","GUY","CLAYTON","HUGH","MAX","DWAYNE","DWIGHT","ARMANDO","FELIX","EVERETT","IAN","WALLACE","KEN","BOB","ALFREDO","ALBERTO","DAVE","IVAN","BYRON","ISAAC","MORRIS","CLIFTON","WILLARD","ROSS","ANDY","SALVADOR","KIRK","SERGIO","SETH","KENT","TERRANCE","EDUARDO","TERRENCE","ENRIQUE","WADE","STUART","FREDRICK","ARTURO","ALEJANDRO","NICK","LUTHER","WENDELL","JEREMIAH","JULIUS","OTIS","TREVOR","OLIVER","LUKE","HOMER","GERARD","DOUG","KENNY","HUBERT","LYLE","MATT","ALFONSO","ORLANDO","REX","CARLTON","ERNESTO","NEAL","PABLO","LORENZO","OMAR","WILBUR","GRANT","HORACE","RODERICK","ABRAHAM","WILLIS","RICKEY","ANDRES","CESAR","JOHNATHAN","MALCOLM","RUDOLPH","DAMON","KELVIN","PRESTON","ALTON","ARCHIE","MARCO","WM","PETE","RANDOLPH","GARRY","GEOFFREY","JONATHON","FELIPE","GERARDO","ED","DOMINIC","DELBERT","COLIN","GUILLERMO","EARNEST","LUCAS","BENNY","SPENCER","RODOLFO","MYRON","EDMUND","GARRETT","SALVATORE","CEDRIC","LOWELL","GREGG","SHERMAN","WILSON","SYLVESTER","ROOSEVELT","ISRAEL","JERMAINE","FORREST","WILBERT","LELAND","SIMON","CLARK","IRVING","BRYANT","OWEN","RUFUS","WOODROW","KRISTOPHER","MACK","LEVI","MARCOS","GUSTAVO","JAKE","LIONEL","GILBERTO","CLINT","NICOLAS","ISMAEL","ORVILLE","ERVIN","DEWEY","AL","WILFRED","JOSH","HUGO","IGNACIO","CALEB","TOMAS","SHELDON","ERICK","STEWART","DOYLE","DARREL","ROGELIO","TERENCE","SANTIAGO","ALONZO","ELIAS","BERT","ELBERT","RAMIRO","CONRAD","NOAH","GRADY","PHIL","CORNELIUS","LAMAR","ROLANDO","CLAY","PERCY","DEXTER","BRADFORD","DARIN","AMOS","MOSES","IRVIN","SAUL","ROMAN","RANDAL","TIMMY","DARRIN","WINSTON","BRENDAN","ABEL","DOMINICK","BOYD","EMILIO","ELIJAH","DOMINGO","EMMETT","MARLON","EMANUEL","JERALD","EDMOND","EMIL","DEWAYNE","WILL","OTTO","TEDDY","REYNALDO","BRET","JESS","TRENT","HUMBERTO","EMMANUEL","STEPHAN","VICENTE","LAMONT","GARLAND","MILES","EFRAIN","HEATH","RODGER","HARLEY","ETHAN","ELDON","ROCKY","PIERRE","JUNIOR","FREDDY","ELI","BRYCE","ANTOINE","STERLING","CHASE","GROVER","ELTON","CLEVELAND","DYLAN","CHUCK","DAMIAN","REUBEN","STAN","AUGUST","LEONARDO","JASPER","RUSSEL","ERWIN","BENITO","HANS","MONTE","BLAINE","ERNIE","CURT","QUENTIN","AGUSTIN","MURRAY","JAMAL","ADOLFO","HARRISON","TYSON","BURTON","BRADY","ELLIOTT","WILFREDO","BART","JARROD","VANCE","DENIS","DAMIEN","JOAQUIN","HARLAN","DESMOND","ELLIOT","DARWIN","GREGORIO","BUDDY","XAVIER","KERMIT","ROSCOE","ESTEBAN","ANTON","SOLOMON","SCOTTY","NORBERT","ELVIN","WILLIAMS","NOLAN","ROD","QUINTON","HAL","BRAIN","ROB","ELWOOD","KENDRICK","DARIUS","MOISES","FIDEL","THADDEUS","CLIFF","MARCEL","JACKSON","RAPHAEL","BRYON","ARMAND","ALVARO","JEFFRY","DANE","JOESPH","THURMAN","NED","RUSTY","MONTY","FABIAN","REGGIE","MASON","GRAHAM","ISAIAH","VAUGHN","GUS","LOYD","DIEGO","ADOLPH","NORRIS","MILLARD","ROCCO","GONZALO","DERICK","RODRIGO","WILEY","RIGOBERTO","ALPHONSO","TY","NOE","VERN","REED","JEFFERSON","ELVIS","BERNARDO","MAURICIO","HIRAM","DONOVAN","BASIL","RILEY","NICKOLAS","MAYNARD","SCOT","VINCE","QUINCY","EDDY","SEBASTIAN","FEDERICO","ULYSSES","HERIBERTO","DONNELL","COLE","DAVIS","GAVIN","EMERY","WARD","ROMEO","JAYSON","DANTE","CLEMENT","COY","MAXWELL","JARVIS","BRUNO","ISSAC","DUDLEY","BROCK","SANFORD","CARMELO","BARNEY","NESTOR","STEFAN","DONNY","ART","LINWOOD","BEAU","WELDON","GALEN","ISIDRO","TRUMAN","DELMAR","JOHNATHON","SILAS","FREDERIC","DICK","IRWIN","MERLIN","CHARLEY","MARCELINO","HARRIS","CARLO","TRENTON","KURTIS","HUNTER","AURELIO","WINFRED","VITO","COLLIN","DENVER","CARTER","LEONEL","EMORY","PASQUALE","MOHAMMAD","MARIANO","DANIAL","LANDON","DIRK","BRANDEN","ADAN","BUFORD","GERMAN","WILMER","EMERSON","ZACHERY","FLETCHER","JACQUES","ERROL","DALTON","MONROE","JOSUE","EDWARDO","BOOKER","WILFORD","SONNY","SHELTON","CARSON","THERON","RAYMUNDO","DAREN","HOUSTON","ROBBY","LINCOLN","GENARO","BENNETT","OCTAVIO","CORNELL","HUNG","ARRON","ANTONY","HERSCHEL","GIOVANNI","GARTH","CYRUS","CYRIL","RONNY","LON","FREEMAN","DUNCAN","KENNITH","CARMINE","ERICH","CHADWICK","WILBURN","RUSS","REID","MYLES","ANDERSON","MORTON","JONAS","FOREST","MITCHEL","MERVIN","ZANE","RICH","JAMEL","LAZARO","ALPHONSE","RANDELL","MAJOR","JARRETT","BROOKS","ABDUL","LUCIANO","SEYMOUR","EUGENIO","MOHAMMED","VALENTIN","CHANCE","ARNULFO","LUCIEN","FERDINAND","THAD","EZRA","ALDO","RUBIN","ROYAL","MITCH","EARLE","ABE","WYATT","MARQUIS","LANNY","KAREEM","JAMAR","BORIS","ISIAH","EMILE","ELMO","ARON","LEOPOLDO","EVERETTE","JOSEF","ELOY","RODRICK","REINALDO","LUCIO","JERROD","WESTON","HERSHEL","BARTON","PARKER","LEMUEL","BURT","JULES","GIL","ELISEO","AHMAD","NIGEL","EFREN","ANTWAN","ALDEN","MARGARITO","COLEMAN","DINO","OSVALDO","LES","DEANDRE","NORMAND","KIETH","TREY","NORBERTO","NAPOLEON","JEROLD","FRITZ","ROSENDO","MILFORD","CHRISTOPER","ALFONZO","LYMAN","JOSIAH","BRANT","WILTON","RICO","JAMAAL","DEWITT","BRENTON","OLIN","FOSTER","FAUSTINO","CLAUDIO","JUDSON","GINO","EDGARDO","ALEC","TANNER","JARRED","DONN","TAD","PRINCE","PORFIRIO","ODIS","LENARD","CHAUNCEY","TOD","MEL","MARCELO","KORY","AUGUSTUS","KEVEN","HILARIO","BUD","SAL","ORVAL","MAURO","ZACHARIAH","OLEN","ANIBAL","MILO","JED","DILLON","AMADO","NEWTON","LENNY","RICHIE","HORACIO","BRICE","MOHAMED","DELMER","DARIO","REYES","MAC","JONAH","JERROLD","ROBT","HANK","RUPERT","ROLLAND","KENTON","DAMION","ANTONE","WALDO","FREDRIC","BRADLY","KIP","BURL","WALKER","TYREE","JEFFEREY","AHMED","WILLY","STANFORD","OREN","NOBLE","MOSHE","MIKEL","ENOCH","BRENDON","QUINTIN","JAMISON","FLORENCIO","DARRICK","TOBIAS","HASSAN","GIUSEPPE","DEMARCUS","CLETUS","TYRELL","LYNDON","KEENAN","WERNER","GERALDO","COLUMBUS","CHET","BERTRAM","MARKUS","HUEY","HILTON","DWAIN","DONTE","TYRON","OMER","ISAIAS","HIPOLITO","FERMIN","ADALBERTO","BO","BARRETT","TEODORO","MCKINLEY","MAXIMO","GARFIELD","RALEIGH","LAWERENCE","ABRAM","RASHAD","KING","EMMITT","DARON","SAMUAL","MIQUEL","EUSEBIO","DOMENIC","DARRON","BUSTER","WILBER","RENATO","JC","HOYT","HAYWOOD","EZEKIEL","CHAS","FLORENTINO","ELROY","CLEMENTE","ARDEN","NEVILLE","EDISON","DESHAWN","NATHANIAL","JORDON","DANILO","CLAUD","SHERWOOD","RAYMON","RAYFORD","CRISTOBAL","AMBROSE","TITUS","HYMAN","FELTON","EZEQUIEL","ERASMO","STANTON","LONNY","LEN","IKE","MILAN","LINO","JAROD","HERB","ANDREAS","WALTON","RHETT","PALMER","DOUGLASS","CORDELL","OSWALDO","ELLSWORTH","VIRGILIO","TONEY","NATHANAEL","DEL","BENEDICT","MOSE","JOHNSON","ISREAL","GARRET","FAUSTO","ASA","ARLEN","ZACK","WARNER","MODESTO","FRANCESCO","MANUAL","GAYLORD","GASTON","FILIBERTO","DEANGELO","MICHALE","GRANVILLE","WES","MALIK","ZACKARY","TUAN","ELDRIDGE","CRISTOPHER","CORTEZ","ANTIONE","MALCOM","LONG","KOREY","JOSPEH","COLTON","WAYLON","VON","HOSEA","SHAD","SANTO","RUDOLF","ROLF","REY","RENALDO","MARCELLUS","LUCIUS","KRISTOFER","BOYCE","BENTON","HAYDEN","HARLAND","ARNOLDO","RUEBEN","LEANDRO","KRAIG","JERRELL","JEROMY","HOBERT","CEDRICK","ARLIE","WINFORD","WALLY","LUIGI","KENETH","JACINTO","GRAIG","FRANKLYN","EDMUNDO","SID","PORTER","LEIF","JERAMY","BUCK","WILLIAN","VINCENZO","SHON","LYNWOOD","JERE","HAI","ELDEN","DORSEY","DARELL","BRODERICK","ALONSO" ]


score :: String -> Int
score []     = 0
score (s:tr) = (ord s - ord 'A') + 1 + score tr







