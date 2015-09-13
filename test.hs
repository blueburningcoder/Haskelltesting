doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallerNumber x = if x > 100 then x else x * 2
conanO'Brien = "I's a-me, Conan O'Brien!"



-- so this is a comment? nice :)
--
doubleMe x = x + x
doubleIfSmall x = if x < 100 then x ^ 2 else x + 1
lastNum = [0,1,2,3,4,5,6,7,8,9] -- /= [0,1..10]
-- [(lastNum !! x) | x <- [0,1..(length lastNum -1)]]
