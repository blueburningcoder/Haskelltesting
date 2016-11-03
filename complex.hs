module Complex where

import Data.Monoid

type R = Double

data Complex = Co R R -- First is the Real part, second is the Imagenary
data Polar = Po R R -- First is the absolute distance, second is the direction in radians
-- needed only later for exponentiation and such


-- | to show them a bit differently than they should be displayed, just the way they should be displayed
instance Show Complex where
    show (Co r i) = show r ++ " " ++ show i ++ "i"

-- | to show the Polar number in the way it should be shown
instance Show Polar where
    show (Po v d) = show v ++ " " ++ show d ++ "r " ++ (show . radToDeg $ d) ++ "º"

-- | Well the actual exercise was to implement the Num-instance, here it is
instance Num Complex where
    (+) = (+:)
    (*) = (*:)
    (-) = (-:)
    abs = cAbs
    signum = sigComplex
    fromInteger = fromIntegral'

-- | well it is a Fractional number for some Reason or another, at least we can divide
instance Fractional Complex where
    (/) = (/:)
    recip = recipComplex
    fromRational = fromRational'

-- | well you can test if two numbers are equal, but not which of them is bigger in any way
instance Eq Complex where
    (Co a b) == (Co c d) = a == c && b == d

-- | this would make as much sense with the multiplication operator and the multiplication id, but you can only instantiate it once
instance Monoid Complex where
    mempty = simpleComplex 0
    mappend = (+:)

-- | converting a Rational number to a Complex one
fromRational' :: Rational -> Complex
fromRational' n = (Co pi 0) 
-- I haven't figured out why this is (exactly) necessary yet
-- but the ccos and csin won't wrok otherwise

-- | converting an Integer to a complex number
fromIntegral' :: Integral a => a -> Complex
fromIntegral' i = simpleComplex . fromIntegral $ i

-- | converting a Real number to a simple Complex one
simpleComplex :: R -> Complex
simpleComplex d = Co d 0

-- | converting two Real numbers to a Complex one
toComplex :: R -> R -> Complex
toComplex r i = Co r i

infixl 9 ~:
(~:) :: R -> R -> Complex
r ~: i = toComplex r i

-- | adding two Complex numbers together
addComplex :: Complex -> Complex -> Complex
addComplex (Co r i) (Co rr ii) = Co (r + rr) (i + ii)

infixl 6 +:
(+:) :: Complex -> Complex -> Complex
r +: i = addComplex r i

-- | multiplying two Complex numbers together
multComplex :: Complex -> Complex -> Complex
multComplex (Co a b) (Co c d) = Co (a * c - b * d) (b * c + a * d)

infixl 8 *:
(*:) :: Complex -> Complex -> Complex
r *: i = multComplex r i

-- | subtracting two Complex numbers from one another
subComplex :: Complex -> Complex -> Complex
subComplex (Co r i) (Co rr ii) = Co (r - rr) (i - ii)

infixl 6 -:
(-:) :: Complex -> Complex -> Complex
r -: i = subComplex r i

-- | conjugating a Complex number
conjugate :: Complex -> Complex
conjugate (Co r i) = Co r (-i)
con = conjugate

-- | dividing two Complex numbers through one another
divComplex :: Complex -> Complex -> Complex
divComplex _ (Co 0 0) = error "can not divide through zero"
divComplex e@(Co a b) f@(Co c d) = Co ((a * c + b * d) / divi) ((b * c - a * d) / divi)
    where divi = c ^ 2 + d ^ 2

infixl 7 /:
(/:) :: Complex -> Complex -> Complex
r /: i = divComplex r i

-- | dividing a Complex number through a Real one
divcomplex :: Complex -> R -> Complex
divcomplex (Co a b) t = Co (a/t) (b/t)

infixl 7 /-
(/-) :: Complex -> R -> Complex
i /- r = divcomplex i r

-- | multiplies a Complex number with a Real one
multcomplex :: Complex -> R -> Complex
multcomplex (Co a b) r = Co (a * r) (b * r)

-- | returns the rounded absolute value of a Complex number
-- rounded because it's end type has to be Num 'a'
cAbs :: (Num a) => Complex -> a
cAbs c = fromIntegral . floor . sqrt $ a
    where (Co a _) = (con c) * c

-- | the unrounded absolute value of a Complex number
cabs :: Complex -> R
cabs c = sqrt a
    where (Co a _) = (con c) * c

-- | returns a simpleComplex number with the result of the signum function
sigComplex :: Complex -> Complex
sigComplex (Co 0 0) = simpleComplex 0
sigComplex a = simpleComplex . signum $ e
    where (Co e _) = a / (cAbs a)

-- | returns a Complex number in a pair of polar coordinates
toPolar :: Complex -> Polar
toPolar a = Po v d
    where v = cabs a; d = arg a

fromPolar :: Polar -> Complex
fromPolar (Po v d) = (exi (Co 0 d)) * (Co v 0)

-- | takes some degrees in radians and returns them in actual degree
radToDeg :: R -> R
radToDeg r = r * 360 / (2 * pi)

-- | returns the argument (phase) of the complex value
arg :: Complex -> R
arg (Co r i) 
    | r == 0 && i >  0 = pi / 2
    | r == 0 && i <  0 = negate pi / 2
    | r <  0 && i <  0 = arc - pi
    | r <  0 && i >= 0 = arc + pi
    | r >  0           = arc
    | otherwise        = undefined
    where arc = atan (i / r)

-- | reciprocalling a Complex number
recipComplex :: Complex -> Complex
recipComplex (Co 0 0) = undefined
recipComplex a = c /: (a *: c)
    where c = con a

-- | the squareroot of a complex number, note that you need to add plusminus yourself
sqrtComplex :: Complex -> Complex
sqrtComplex (Co a b) = Co (sqrt $ (a + c) / 2) ((signum b) * sqrt (((-a) + c) / 2))
    where c = sqrt $ a * a + b * b

-- returns the next Number from the Mandelbrot-se
mandelNext :: Complex -> Complex -> Complex
mandelNext z c = z * z + c

-- If this complex result with this initial complex number and so many iterations left is part of the Mandelb
mandelElem :: Complex -> Complex -> Int -> Bool
mandelElem z _ 0 = cabs z <= 2
mandelElem z c n
    | cabs z <= 2 = mandelElem (mandelNext z c) c (n-1)
    | otherwise = False

-- if this complex number is part of the Mandelbrot-set or not (with a fixed number of iterations)
isElem :: Complex -> Bool
isElem z = mandelElem z z 12

-- the delta for the x- and y-axis
dx = 1 - (3 / 81)
dy = 1 - (2 / 26)

-- the values for each of the points to be plotted
xrange = reverse [1,dx..(-2)]
yrange = [1,dy..(-1)]
crange = [toComplex x y | y <- yrange, x <- xrange]

-- splitting and printing it correctly
printing :: [Bool] -> IO ()
printing [] = putStrLn "\nFinished plotting the Mandelbrot set"
printing (l:li) = do
        putChar c
        if length li `mod` 82 == 0 then putStr "\n" else putStr ""
        printing li
    where c = if l then '*' else '.'

-- plotting the Mandelbrot-set in 82x27-style
plotMandel :: IO ()
plotMandel = printing $ map isElem crange

-- recipComplex :: Complex -> Complex

fac :: Integral a => a -> a
fac 0 = 1
fac n = n * fac (n-1)


e :: R
e = 2.7182821828245

{-
esum :: Num a => (a -> a) -> a
esum f = sum f 100
    where sum f n = sum f (n-1) + f n
-}

ex :: R -> R
ex = (e**)

-- | e ^ (a + bi)
exi :: Complex -> Complex
exi (Co 0 i) = Co (cos i) (sin i)
exi (Co r 0) = Co (ex r) 0
exi (Co r i) = exi (Co r 0) * exi (Co 0 i)

csin :: R -> R
csin z = r
    where (Co r i) = (1 / (Co 0 2)) * (exi (Co 0 z) - exi (Co 0 (-z))) / (Co (2 * pi) 0)

ccos :: R -> R
ccos z = r
    where (Co r i) = 0.5 * (exi (Co 0 z) + exi (Co 0 (-z))) / (Co (2 * pi) 0)






-- some test-complex numbers
complexTest  = Co 3 5
complexTest2 = Co 5 3
complexTest3 = Co 7 2
complexTest4 = Co 2.5 3.3
complexTest5 = Co (-2) (-5)

