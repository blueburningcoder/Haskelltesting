import Data.Char
import Text.Printf



fromMessage :: String -> String
fromMessage m = zipWith change (concat $ o >>= (\n -> [printf "%b" n :: [Char]])) s
  where
  o = map ord m


c = concat ["1101001","1100011","1101000","1100010","1101001","1101110","1100010","1100001","1110100","1101101","1100001","1101110"]
m = concat ["1001001","1000011","1001000","1001100","1001001","1000101","1000010","1000101","1000100","1001001","1000011","1001000"]
s = concat ["1415926","5358979","3238462","6433832","7950288","4197169","3993751","0582097","4944592","3078164","0628620","8998628"]



change a b = case a of
    '0' -> b
    '1' -> case b of
            '9' -> '8'
            n -> chr $ (ord n) + 1


e = zipWith change m s



splitting :: [a] -> Int -> [[a]]
splitting [] _ = []
splitting li n = take n li : splitting (drop n li) n


{-

141592653589
241693653588
100101000001

100101 000001
1001 0100 0001  -- not
100 101 000 001


-}


mes = ["100", "101", "000", "001"]

decode :: String -> Int
decode = map (actual 0)
  where
  actual _ []  = 0
  actual n ('0':r) = 0 + actual (n+1) r
  actual n ('1':r) = 2^n + actual (n+1) r


