module Voting where 

import System.Random



type Candidate = String
type AllCandidates = [Candidate]
type Rating = [Candidate]
type Voters = [Rating]



getRandomString :: RandomGen a => a -> Int -> String
getRandomString _ 0 = []
getRandomString g n = do
    let (g1, g2) = split $ g
    (++) (take 1 . randomRs ('a', 'z') $ g1) (getRandomString g2 (n-1))


getRandomRating :: RandomGen a => a -> AllCandidates -> Rating
getRandomRating _ [] = []
getRandomRating g c = let
    (g1, g2) = split $ g
    chosen = c !! (head $ randomRs (0, length c -1) $ g1)
    in chosen:(getRandomRating g2 (filter (\b -> b /= chosen) c))


candidates = ["A", "B", "C", "D"]


