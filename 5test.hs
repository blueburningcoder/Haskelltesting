{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

-- required later
import qualified Control.Category as C
import Control.Arrow
import Data.Monoid ((<>))


-- import Control.Parallel.Strategies (using, parList, rpar)
import Control.Parallel.Strategies



class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
  data Move pokemon :: *
  pickMove :: pokemon -> Move pokemon

data Fire = Charmander | Charmeleon | Charizard deriving Show
instance Pokemon Fire where
  data Move Fire = Ember | FlameThrower | FireBlast deriving Show
  pickMove Charmander = Ember
  pickMove Charmeleon = FlameThrower
  pickMove Charizard = FireBlast

data Water = Squirtle | Wartortle | Blastoise deriving Show
instance Pokemon Water where
  data Move Water = Bubble | WaterGun deriving Show
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
instance Pokemon Grass where
  data Move Grass = VineWhip deriving Show
  pickMove _ = VineWhip

printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne ++ " used " ++ moveOne
  putStrLn $ pokemonTwo ++ " used " ++ moveTwo
  putStrLn $ "Winner is: " ++ winner ++ "\n"

class (Show (Winner pokemon foe), Pokemon pokemon, Pokemon foe) => Battle pokemon foe where
  type Winner pokemon foe :: *
  type Winner pokemon foe = pokemon

  battle :: pokemon -> foe -> IO ()
  battle pokemon foe = do
    printBattle (show pokemon) (show move) (show foe) (show foeMove) (show winner)
   where
    foeMove = pickMove foe
    move = pickMove pokemon
    winner = pickWinner pokemon foe

  pickWinner :: pokemon -> foe -> (Winner pokemon foe)

instance Battle Water Fire where
  pickWinner pokemon foe = pokemon

instance Battle Fire Water where
  type Winner Fire Water = Water
  pickWinner = flip pickWinner

instance Battle Grass Water where
  pickWinner pokemon foe = pokemon

instance Battle Water Grass where
  type Winner Water Grass = Grass
  pickWinner = flip pickWinner

instance Battle Fire Grass where
  pickWinner pokemon foe = pokemon

instance Battle Grass Fire where
  type Winner Grass Fire = Fire
  pickWinner = flip pickWinner

{-
main :: IO ()
main = do
  battle Squirtle Charmander
  battle Charmeleon Wartortle
  battle Bulbasaur Blastoise
  battle Wartortle Ivysaur
  battle Charmeleon Ivysaur
  battle Venusaur Charizard
-- -}





--- Arrowtesting :)

-- SF = Stream-Function
newtype SF a b = SF { runSF :: [a] -> [b] }

instance C.Category SF where
  id = SF (map C.id)
  (SF a) . (SF b) = SF (a . b)

instance Arrow SF where
  arr f = SF (map f)
  first (SF f) = SF (unzip >>> first f >>> uncurry zip)

instance ArrowChoice SF where
  left (SF f) = SF (\xs -> combine xs (f [y | Left y <- xs]))
    where combine (Left y:xs) (z:zs) = Left z: combine xs zs
          combine (Right y:xs) zs = Right y: combine xs zs
          combine [] zs = []

instance ArrowLoop SF where
  loop (SF f) = SF $ \as ->
      let (bs, cs) = unzip (f (zip as (stream cs))) in bs
    where stream ~(x:xs) = x:stream xs

delay :: b -> SF b b
delay x = SF (init . (x:))

listcase :: [a] -> Either () (a, [a])
listcase []     = Left ()
listcase (x:xs) = Right (x,xs)

mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f =
    arr listcase >>>
    arr (const []) |||
      (f *** mapA f >>> arr (uncurry (:)))


delaysA :: SF [a] [a]
delaysA =
    arr listcase >>>
    arr (const []) |||
      (arr id *** (delaysA >>> delay []) >>> arr (uncurry (:)))

truecase :: Bool -> a -> Either () a
truecase False _ = Left ()
truecase True  a = Right a

uncase :: (Either b a, [a]) -> [a]
uncase (Left  _, xs) = xs
uncase (Right x, xs) = x:xs

-- {-

-- TODO: Exercise!
-- behave like filter ^=^ replacement
-- FIXME: behave like filterM on Kleisli-Arrows (?)
-- Tipp?: Streams (delay?) (listcase?) (mapA?)
filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA f = arr listcase >>> -- [a] -> Either () (a,[a])
    arr (const []) ||| -- case of empty list: return empty list
      ( first (f &&& arr id) >>>
      arr (uncurry truecase) *** filterA f >>>
      arr uncase )

-- -}

{-
StreamProcessors: Put b f represents a stream processor that is ready to output b and
continue with f, and Get k represents a stream processor waiting for an
input a, which will continue by passing it to k.
-}


-- SP = Stream Processors
data SP a b = Put b (SP a b)
            | Get (a -> SP a b)

runSP :: SP a b -> [a] -> [b]
runSP (Put b s) as = b:runSP s as
runSP (Get k) (a:as) = runSP (k a) as
runSP (Get k) [] = []

instance C.Category SP where
  id  = Get (\a -> Put a C.id)
  sa . sb = undefined

{-
  (Get a)   . (Get c)   = undefined
  (Put a b) . (Get c)   = undefined
  (Get a)   . (Put c d) = undefined
  (Put a b) . (Put c d) = undefined
--  -}
-- bc -> ab -> ac
-- bc . ab = ac


instance Arrow SP where
  arr f = Get (\a -> Put (f a) (arr f) )
  first = undefined

instance ArrowChoice SP where
  left = undefined

instance ArrowLoop SP where
  loop = undefined


-- instance ArrowCircuit SP


-- -}
-- -}




--------------------------------------------------------------------
-----                   Some Arrow-Logic-Stuff                ------
--------------------------------------------------------------------


type Logic = SF (Bool, Bool) Bool

lnor :: Logic
lnor = arr (not . uncurry (||) )

lor :: Logic
lor = arr $ uncurry (||)

land :: Logic
land = arr $ uncurry (&&)

lnand :: Logic
lnand = arr $ (not . uncurry (&&) )

getOutput :: Logic -> (Bool, Bool) -> Bool
getOutput l i = head $ (runSF l) [i]

pprettyPrint :: [(Bool,Bool)] -> Logic -> IO ()
pprettyPrint inp l = do
  putStrLn f
  putStrLn s
  putStrLn $ (\(a,b) -> a ++ "\n" ++ b) $ pPrintBool res
  where
    res :: [Bool]
    res = mapA (arr (getOutput l)) $ inp
    f = (\(a,b) -> a ++ "\n" ++ b) $ pPrintBool $ map fst inp
    s = (\(a,b) -> a ++ "\n" ++ b) $ pPrintBool $ map snd inp

pPrintBool :: [Bool] -> (String, String)
pPrintBool (x:xs) = (map xtrue (x:xa), map (xtrue . not) (x:xa)) <> (" ", "|") <> pPrintBool xd
  where xtrue b = if b then '_' else ' '
        (xa,xd) = break (/=x) xs
pPrintBool _ = ("", "")

edge :: SF Bool Bool
edge = arr id &&& delay False >>> arr detect
  where detect (a,b) = a && not b


tB1, tB2 :: [Bool]
tB1 = [True, True, False, False, True, True, False, False]
tB2 = [True, False, False, True, True, False, False, True]
tBs = zip tB1 tB2







-- N - 2000 ist eine Zweierpotenz
-- N ist die differenz zweier zweierpotenzen
-- Bestimme Jahreszahlen N


isTwoExp :: Int -> Bool
isTwoExp = go 0
  where
    go :: Int -> Int -> Bool
    go a b | b == 2 ^ a = True
           | b  < 2 ^ a = False
           | otherwise  = go (a + 1) b

twoExp :: [Int]
twoExp = [ 2 ^ n | n <- [0..62]]

twodiff :: Int -> Bool
twodiff = undefined


result = [ n | n <- [2000,2002..], isTwoExp n && isTwoExp (n - 2000)]





-- main = print (result `using` parList rpar)


main = undefined
