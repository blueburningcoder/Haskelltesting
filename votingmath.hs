module Voting where 

import System.Random
import Data.List            (delete)
import Control.Applicative  ((<$>))
import Control.Monad        ((>>=), return)


type Candidate = String
type AllCandidates = [Candidate]
type Rating = [Candidate]
type Voting = [Rating]

-- returns a random string of length n based on the RandomGen g
getRandomString :: RandomGen a => a -> Int -> String
getRandomString _ 0 = []
getRandomString g n = do
    let (g1, g2) = split $ g
    (++) (take 1 . randomRs ('a', 'z') $ g1) (getRandomString g2 (n-1))


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


-- returns the absolute winner on first place
absolute :: Voting -> Maybe Candidate
absolute [] = Nothing
absolute a@(v:vo) = (relative a) >>= (\c -> if atPos head a c > (length a) `div` 2 then return c else Nothing)

-- returns the relative winner on first place
relative :: Voting -> Maybe Candidate
relative [] = Nothing
relative a@(v:vo) = (soleMax list >>= (getIndexOf list)) >>= return . (v !!)
    where list = [atPos head a c | c <- v]

-- returns the sole maximum of a list if existent
soleMax :: (Num a, Ord a, Eq a) => [a] -> Maybe a
soleMax [] = Nothing
soleMax l  = if (length $ filter (\n -> n == m) l) > 1 then Nothing else Just m
    where m = maximum l

-- returns the index of an element from a list if it is an element
getIndexOf :: Eq a => [a] -> a -> Maybe Int
getIndexOf [] _     = Nothing
getIndexOf (l:li) e = if l == e then Just 0 else (1+) <$> (getIndexOf li e)

-- returns how often a candidate is at a certain position in a poll
atPos :: (Rating -> Candidate) -> Voting -> Candidate -> Int
atPos _ [] _ = 0
atPos f (v:vo) c = if (f v) == c then 1 + ed else 0 + ed
    where ed = atPos f vo c





-- generating a reproducable random dataset for testing
genBegin = 15

candidates = ["A", "B", "C", "D", "E", "F"]
voting = let
    gen = mkStdGen genBegin
    (g, _) = split $ gen
    in getRandomVoting g 30 candidates

