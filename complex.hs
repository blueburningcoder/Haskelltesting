module Complex where


type R = Double

data Complex = Co R R


instance Show Complex where
    show (Co r i) = show r ++ " " ++ show i ++ "i"

instance Num Complex where
    (+) = (+:)
    (*) = (*:)
    (-) = (-:)
    abs = cAbs
    signum = undefined
    fromInteger = fromInteger'


-- | converting an Integer to a complex number
fromInteger' :: Integer -> Complex
fromInteger' i = simpleComplex . fromInteger $ i

-- | converting a Real number to a simple Complex one
simpleComplex :: Double -> Complex
simpleComplex d = Co d 0

-- | converting two Real numbers to a Complex one
toComplex :: Double -> Double -> Complex
toComplex r i = Co r i

infixl 9 ~:
(~:) :: Double -> Double -> Complex
r ~: i = toComplex r i

-- | adding two Complex numbers together
addComplex :: Complex -> Complex -> Complex
addComplex (Co r i) (Co rr ii) = Co (r + rr) (i + ii)

infixl 6 +:
(+:) :: Complex -> Complex -> Complex
r +: i = addComplex r i

-- | multiplying two Complex numbers together
multComplex :: Complex -> Complex -> Complex
multComplex (Co r i) (Co rr ii) = Co (r * rr - i * ii) (r * i + rr * ii)

infixl 7 *:
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
divComplex e@(Co a b) f@(Co c d) =  (e *: (con f)) /- (c^2 + d^2)

infixl 7 /:
(/:) :: Complex -> Complex -> Complex
r /: i = divComplex r i

-- | dividing a Complex number through a Real one
divcomplex :: Complex -> R -> Complex
divcomplex (Co a b) t = Co (a/t) (b/t)

infixl 7 /-
(/-) :: Complex -> Double -> Complex
i /- r = divcomplex i r

cAbs :: (Num a) => Complex -> a
cAbs c = fromIntegral . floor $ a
    where (Co a b) = (con c) *: c





complexTest  = Co 3 5
complexTest2 = Co 5 3
complexTest3 = Co 7 2
complexTest4 = Co 2.5 3.3
