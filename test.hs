-- so this is a comment? nice :)
--
doubleMe x = x + x
doubleIfSmall x = if x < 100 then x ^ 2 else x + 1
lastNum = [1,2,3,4,5,6,7,8,9,0]
-- [(lastNum !! x) | x <- [0,1..(length lastNum -1)]]
