
module Hw_geoclasses where

import Text.Printf

class TDO a where
  x :: (TDO a, Num b) => a -> b
  y :: (TDO a, Num b) => a -> b

data TwoDObject a = TDOb a a
  deriving(Eq)

instance (Show a, Num a, PrintfArg a) => Show (TwoDObject a) where
  show (TDOb x y) = printf "TwoDObject(x=%d, y=%d)" x y

instance TDO (TwoDObject a) where
  x (TDOb x' _) = x'
  y (TDOb _ y') = y'


data Circle a = C (TwoDObject a) a
  deriving Eq

instance (Show a, Num a, PrintfArg a) => Show (Circle a) where
  show (C (TDOb x y) r) = printf "Circle(x=%d, y=%d, r=%d)" x y r

-- instance TwoDObject (Circle a) where

area :: (Fractional a) => Circle a -> a
area (C _ r) = 3.14159265357898 * r * r

change_size :: Fractional a => Circle a -> a -> Circle a
change_size (C p r) c = C p (r * c)


data Rectangle a = R (TwoDObject a) a a
  deriving Eq

instance (Show a, PrintfArg a) => Show (Rectangle a) where
  show (R (TDOb x y) h w) = printf "Rectangle(x=%d, y=%d, height=%d, width=%d)" x y h w



