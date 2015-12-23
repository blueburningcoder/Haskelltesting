module Complex where


type R = Double

data Complex = Co R R


instance Show Complex where
    show (Co r i) = show r ++ " " ++ show i ++ "i"


simpleComplex :: Double -> Complex
simpleComplex d = Co d 0

toComplex :: Double -> Double -> Complex
toComplex r i = Co r i

infixl 9 +:
(+:) :: Double -> Double -> Complex
r +: i = toComplex r i

complexAdd :: Complex -> Complex -> Complex
complexAdd (Co r i) (Co rr ii) = Co (r + rr) (i + ii)

complexMult :: Complex -> Complex -> Complex
complexMult (Co r i) (Co rr ii) = Co (r * rr + i * ii) (r * i + rr * ii)

complexSub :: Complex -> Complex -> Complex
complexSub (Co r i) (Co rr ii) = Co (r - rr) (i - ii)

conjugate :: Complex -> Complex
conjugate (Co r i) = Co r (-i)

complexDiv :: Complex -> Complex -> Complex
complexDiv _ (Co 0 0) = error "can not divide through zero"
complexDiv e@(Co a b) f@(Co c d) = complexdiv (complexMult e (conjugate f)) (c^2 + d^2)


complexdiv :: Complex -> R -> Complex
complexdiv (Co a b) t = Co (a/t) (b/t)





complexTest  = Co 3 5
complexTest2 = Co 5 3
complexTest3 = Co 7 2
