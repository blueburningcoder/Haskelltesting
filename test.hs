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


head' :: [a] -> a
head' [] = error "Empty list ..."
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs  -- NO square brackets, only the round ones (Necessary)


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height                             -- NO = at the end of this line!!!
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're uly!"
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
head' xs = case of [] -> error "No head for empty lists!"
                   (x:_) -> x

























