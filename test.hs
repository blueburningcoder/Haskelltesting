import Data.List
import qualified Data.Set as Set



doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallerNumber :: Int -> Int
doubleSmallerNumber x = if x > 100 then x else x * 2

conanO'Brian :: [Char]
conanO'Brian = "I's a-me, Conan O'Brien!"


-- so this is a comment? nice :)
doubleIfSmall :: Int -> Int
doubleIfSmall x = if x < 100 then x ^ 2 else x + 1

lastNum :: [Int]
lastNum = [0,1,2,3,4,5,6,7,8,9] -- == [0..10]
-- [(lastNum !! x) | x <- [0,1..(length lastNum -1)]]

values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]


lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!!"
lucky x = "Sorry, you're out of luck, pal"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charname :: Char -> String
charname 'a' = "Albert"
charname 'b' = "Broseph"
charname 'c' = "Cecil"
charname c = "Oh. I don't know someone like that."



addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b) -- OLD!!
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c



length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs  -- NO square brackets, only the round ones (Necessary)


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height                             -- NO = at the end of this line!!!
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <=  fat   = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
-- short for for: 
-- bmi :: a -> a -> a
-- bmi weight height = weight / height ^ 2
--
-- even shorter:
-- calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

head' :: [a] -> a

head' [] = error "No head for empty lists!"
head' (x:_) = x
-- equals:
-- head' xs = case xs of [] -> error "No head for empty lists!"
--                      (x:_) -> x


describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty"
          what [x] = "a singleton list"
          what xs = "a longer list"


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted



zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- span ~ takeWhile
-- break = span (not . p)


on :: (b -> b -> c) -> (a -> b) -> a -> a-> c
on f g = \x y -> f (g x) (g y)
-- from Data.Function


findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs) = if k == key then Just v else findKey key xs

-- findKey key  = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing


text1 = "I just had an anime dream. Anime ... Reality ... Are they so different?"

text2 = "The old man left his garbage can out and now his trash is all over my lawn!"




-- module Shapes (Point(..), Shape(..), surface, nudge, baseCircle, baseRect) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

refPoint = Point 0 0

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1)


pointAdd :: Point -> Float -> Float -> Point
pointAdd (Point x y) x2 y2 = Point (x + x2) (y + y2)

nudge :: Shape -> Float -> Float -> Shape -- 'schubsen' -> to move
nudge (Circle p r) a b = Circle (pointAdd p a b) r
nudge (Rectangle p1 p2) a b = Rectangle (pointAdd p1 a b) (pointAdd p2 a b)

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) (Point w h)



data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)







maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs



flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | even n = n:chain (div n 2)
    | odd n = n:chain (n*3 + 1)







