module Voting where 

import System.Random        (mkStdGen, split, randomRs, RandomGen (..))
import Data.List            (delete)
import Control.Applicative  ((<$>))
import Control.Monad        ((>>=), return)
import Data.Maybe           (fromJust)

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
relative a@(v:vo) = (soleMax list >>= (getIndexOf' list)) >>= return . (v !!)
    where list = [atPos head a c | c <- v]

-- returns the sole maximum of a list if existent, if not only maximum then Nothing
soleMax :: (Num a, Ord a, Eq a) => [a] -> Maybe a
soleMax [] = Nothing
soleMax l  = if (length $ filter (\n -> n == m) l) > 1 then Nothing else Just m
    where m = maximum l

-- returns the index of an element from a list if it is an element
getIndexOf :: Eq a => a -> [a] -> Maybe Int
getIndexOf _ []     = Nothing
getIndexOf e (l:li) = if l == e then Just 0 else (1+) <$> (getIndexOf e li)

getIndexOf' = flip getIndexOf

-- returns how often a candidate is at a certain position in a poll
atPos :: (Rating -> Candidate) -> Voting -> Candidate -> Int
atPos _ [] _ = 0
atPos f (v:vo) c = if (f v) == c then 1 + ed else 0 + ed
    where ed = atPos f vo c

-- removes a Maybe Candidate from the voting-list after he got counted
clearOf :: Maybe Candidate -> Voting -> Voting
clearOf _ [] = []
clearOf Nothing vo = vo
clearOf ca@(Just c) (v:vo) = (delete c v):(clearOf ca vo)

-- clears a list from any empty lists within
-- (needed for the last iteration of the hare-algorithm ... )
empty :: [[a]] -> [[a]]
empty [] = []
empty ([]:c) = empty c
empty ((a:b):c) = ((a:b):empty c)

-- uses the hare-method for deciding the winner
hare :: Voting -> Rating
hare [] = []
hare l@(v:vo) = reverse $ (fromJust toRemove):(reverse . hare . empty $ newVoting)
    where
    list = [atPos head l c | c <- v]
    toRemove = (getIndexOf (minimum list) list) >>= return . (v !!)
    newVoting = clearOf toRemove l


-- generating a reproducable random dataset for testing
genBegin = 15

candidates = ["A", "B", "C", "D", "E"]
voting = let
    gen = mkStdGen genBegin
    (g, _) = split $ gen
    in getRandomVoting g 30 candidates

