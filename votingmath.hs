module Voting where 

import System.Random        (newStdGen, mkStdGen, split, randomRs, RandomGen (..))
import Data.List            (delete)
import Control.Applicative  ((<$>))
import Control.Monad        ((>>=), return)
import Data.Maybe           (fromJust, fromMaybe, isJust)

import Debug.Trace          (trace)



type Candidate = String
type AllCandidates = [Candidate]
type Rating = [Candidate]
type Voting = [Rating]


-- returns a random string of length n based on the RandomGen g
getRandomString :: RandomGen a => a -> Int -> String
getRandomString _ 0 = []
getRandomString g n = do
  let (g1, g2) = split $ g
  (++) (take 1 . randomRs ('A', 'z') $ g1) (getRandomString g2 (n-1))

-- returns a random Rating based on the RandomGen g and the list of candidates
getRandomRating :: RandomGen a => a -> AllCandidates -> Rating
getRandomRating _ [] = []
getRandomRating g c = let
  (g1, g2) = split $ g
  chosen = c !! (head $ randomRs (0, length c -1) $ g1)
  in chosen:(getRandomRating g2 (delete chosen c))

-- returns a random Voting from n voters based on the RandomGen g and the list of candidates
getRandomVoting :: RandomGen a => a -> Int -> AllCandidates -> Voting
getRandomVoting _ 0 _ = []
getRandomVoting g n c = let
  (g1, g2) = split $ g
  rating = getRandomRating g1 c
  in rating:(getRandomVoting g2 (n-1) c)

-- returns a list containing n candidates, based on the RandomGen g
getRandomCandidates :: RandomGen a => a -> Int -> AllCandidates
getRandomCandidates _ 0 = []
getRandomCandidates g n = name:(getRandomCandidates g2 (n-1))
  where
  (g1, g2) = split $ g
  name = getRandomString g1 12



-- returns the absolute winner on first place
absolute :: Voting -> Maybe Candidate
absolute [] = Nothing
absolute a@(v:vo) = (relative a) >>= (\c -> if atPos head a c > (length a) `div` 2 then return c else Nothing)

-- returns the relative winner on first place
relative :: Voting -> Maybe Candidate
relative [] = Nothing
relative a@(v:vo) = (soleMax list >>= (getIndexOf' list)) >>= return . (v !!)
  where list = [atPos head a c | c <- v]

-- returns the sole maximum of a list if existent, if not only maximum then Nothing
soleMax :: (Num a, Ord a, Eq a) => [a] -> Maybe a
soleMax [] = Nothing
soleMax l  = if (length $ filter (\n -> n == m) l) > 1 then Nothing else Just m
  where m = maximum l

{-
-- returns the winner of a voting being held in turns
rolling :: Voting -> Rating
rolling (v:vo) = undefined
-}

-- returns the index of an element from a list if it is an element
getIndexOf :: Eq a => a -> [a] -> Maybe Int
getIndexOf _ []   = Nothing
getIndexOf e (l:li) = if l == e then Just 0 else (1+) <$> (getIndexOf e li)

getIndexOf' = flip getIndexOf

-- returns how often a candidate is at a certain position in a poll
atPos :: (Rating -> Candidate) -> Voting -> Candidate -> Int
atPos _ [] _   = 0
atPos f (v:vo) c = if (f v) == c then 1 + ed else 0 + ed
    where ed = atPos f vo c

-- returns the item from the second list that has the same index as the given item in the first list
getItemAt :: Eq a => a -> [a] -> [b] -> Maybe b
getItemAt b bi c = getIndexOf b bi >>= return . (c !!)


-- sorts a second list the same way it would sort the first list
sort2 :: Ord a => [a] -> [b] -> [b]
sort2 [] [] = []
sort2 (l:li) (c:ca) = (sort2 sml sma) ++ [c] ++ (sort2 bgl big)
  where
  sml = [d | d <- li, d < l]
  sma = map fromJust . map ((flip . flip getItemAt) li ca) $ sml
  bgl = [d | d <- li, d > l]
  big = map fromJust . map ((flip . flip getItemAt) li ca) $ bgl


-- removes a Maybe Candidate from the voting-list after he got counted
clearOf :: Maybe Candidate -> Voting -> Voting
clearOf _ [] = []
clearOf Nothing vo = vo
clearOf ca@(Just c) (v:vo) = (delete c v):(clearOf ca vo)

-- clears a list from any empty lists within
-- (needed for the last iteration of the hare-algorithm ... )
empty :: [[a]] -> [[a]]
empty = filter $ not . null

-- uses the hare-method for deciding the overall ranking
hare :: Voting -> Rating
hare [] = []
hare l@(v:vo) = reverse $ (fromJust toRemove):(reverse . hare . empty $ newVoting)
  where
  list = [atPos head l c | c <- v]
  toRemove = (getIndexOf (minimum list) list) >>= return . (v !!)
  newVoting = clearOf toRemove l


-- uses the coombs-method for deciding the overall ranking
coombs :: Voting -> Rating
coombs [] = []
coombs l@(v:vo) = if isJust absol then (fromJust absol):"...":(fromJust toRemove):[] else (reverse $ (fromJust toRemove):(reverse . coombs . empty $ newVoting))
  where
  absol = absolute l
  list = [atPos last l c | c <- v]
  toRemove = (getIndexOf (maximum list) list) >>= return . (v !!)
  newVoting = clearOf toRemove l


{-
 - French, condorcet, spoken: "condorsei", [kɒndɔːrˈseɪ]
 -}


-- orders two candidates not only on one rating but on all of them
condorcetPair :: Candidate -> Candidate -> Voting -> Double
-- condorcetPair can opp v = ((trace ((show res) ++ " " ++ (show . sum $ res) ++ " " ++ (show . length $ res))) (sum res)) `compare` ((fromIntegral $ length res) - sum res)
condorcetPair can opp v = sum res - 15
  where
  list = [condorcetOrdering can opp r | r <- v]
  res = [case l of LT -> 0.0; EQ -> 0.5; GT -> 1.0 | l <- list] :: [Double]

-- orders two candidates based on the given rating
condorcetOrdering :: Candidate -> Candidate -> Rating -> Ordering
condorcetOrdering can opp rat = ordcan `compare` ordopp
  where
  last = fromMaybe (length rat)
  ordcan = last $ getIndexOf can rat >>= (\n -> return $ length rat - n)
  ordopp = last $ getIndexOf opp rat >>= (\n -> return $ length rat - n)

-- decides the condcetWinner if there is one
condorcetWinner :: Voting -> Rating
condorcetWinner v = if length resu /= length (head v) then "...":resu else resu
  where
  pair = [(c,condorcetPair c o v,o) | c <- (head v), o <- (head v), o /= c]
  summ = [filter (\(c,r,o) -> c == d) pair | d <- (head v)]
  list = [sum . map (\(c,r,o) -> r) $ d | d <- summ]
  resu = reverse . sort2 list $ (head v)


{-
-- there's a bucklin-method too
bucklin :: Voting -> Int -> Maybe Rating
bucklin _ 0 = Nothing
bucklin v i | length (head v) < i = Nothing
bucklin v i = trace ( show summ ) undefined
  where
  positions = [0..(i-1)]
  summ = [sum [atPos (!!m) v c | m <- positions] | c <- (head v)]
-}

bucklin :: Voting -> Maybe Rating
bucklin v = undefined


-- borda-method, one might know it from certain sports
borda :: Voting -> Rating
borda v = trace ( show (head v) ++ "\n" ++ show summ ++ "\n") sorted
  where
  positions = [0..(length (head v) -1)]
  summ = [ sum [(atPos (!!m) v c) * (length positions -m) | m <- positions] | c <- (head v)]
  sorted = reverse $ sort2 summ (head v)




-- generating a reproducable random dataset for testing
(gen1, gen2) = split $ mkStdGen 1698651348996434
genBegin = 15
amountOfCandidates = 30

-- a list of candidates and a reproducable (...) voting
candidates = ["A", "B", "C", "D", "E", "F", "G", "H"]
voting = let
  gen = mkStdGen genBegin
  (g, _) = split $ gen
  in getRandomVoting g amountOfCandidates candidates

-- generating a truly random dataset
newVoting :: AllCandidates -> Int -> IO Voting
newVoting c n = do
  g <- newStdGen
  return . getRandomVoting g n $ c

-- this is a situation the hare makes the better decision (in comparision to relative ...)
hare_test = (take 29 . repeat $ ["A", "B", "C"]) ++ (take 31 . repeat $ ["B", "A", "C"]) ++ (take 40 . repeat $ ["C", "A", "B"])


finished_voting_to_ratings = ["relative", "absolute", "hare", "coombs", "condorceit", " 'bucklin' ", "borda"]

