-- returns the biggest divisor of both numbers, a > b does not matter
euklidian :: Integral a => a -> a -> a
euklidian a 0 = a
euklidian a b = euklidian b $ a - div a b * b

-- | taking a Number and returning a list of Numbers this nambers this number can be diveded through evenly
getPrimeParts :: Int -> [Int]
getPrimeParts a = getPrimeParts' a (a-1)

-- | taking two numbers, and tests if they can be devided through each other evenly
getPrimeParts' :: Int -> Int -> [Int]
getPrimeParts' a b
    | b == 1 = []
    | otherwise = if a `mod` b == 0 then b:c else c
    where c = getPrimeParts' a (b-1)

-- | taking a List with elements, and a second list of elements and xors them in their order
anti :: Eq a => [a] -> [a] -> [a]
anti [] l = l
anti l [] = l
anti (a:b) (c:d) = if a == c then anti b d else c:(anti (a:b) d)

-- Euler's phi-function
-- Für jede positive ganze Zahl n bezeichnet φ(n) die 
-- Anzahl der zu n teilerfremden Zahlen k, für die 1 <= k < n gilt.
-- Diese Funktion φ heiẞt euler'sche φ-Funktion

-- Sei p eine Primzahl und seien k, m, n positive ganze Zahlen. Dann gilt:
-- φ(p) = p - 1  -- check
-- φ(p^k) = p^(k-1)*p^k = p^k - p^(k-1)  -- check
-- Wenn ggT(m,n) = 1 dann ist φ(mn) = φ(m)*φ(n) -- mn ? m*n? m+n? no check
--
φ :: Int -> Int
φ a = length . filter (\n -> euklidian a n == 1) $ [1..a]

-- | the debug version, showing each of the elements
φ' :: Int -> [Int]
φ' a = filter (\n -> euklidian a n == 1) $ [1..a]
--
