{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}


{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}


import GHC.Exts


import Data.Map
import Data.IntMap


data WithShow x where
  WithShow :: Show a => a -> WithShow a

bar :: WithShow a -> String
bar (WithShow x) = show x

-- WithShow :: * -> *

-- instance Functor (Either a) where ...

instance Show (WithShow a) where
  show x = "WithShow (" ++ bar x ++ ")"

data WithTypeClass :: (* -> Constraint) -> * -> * where
  WithTypeClass :: c a => a -> WithTypeClass c a

bar' :: WithTypeClass Show a -> String
bar' (WithTypeClass x) = show x

data WithMonadLikeConstraint :: ((* -> *) -> Constraint) -> (* -> *) -> * -> * where
  WithMonadLikeConstraint :: c m => m a -> WithMonadLikeConstraint c m a

instance Functor (WithMonadLikeConstraint Functor f) where
  fmap f (WithMonadLikeConstraint x) = WithMonadLikeConstraint (fmap f x)

data WithGeneralConstraint :: (k -> Constraint) -> k -> (k -> *) -> * where
  WithGeneralConstraint :: c k => i k -> WithGeneralConstraint c k i

newtype Id a = Id { getId :: a }

bar'' :: WithGeneralConstraint Show a Id -> String
bar'' (WithGeneralConstraint (Id x)) = show x

newtype Apply a f = Apply (f (a))
newtype MkFunctorial f a = MkFunctorial (WithGeneralConstraint Functor f (Apply a))

instance Functor (MkFunctorial f) where
  fmap f (MkFunctorial (WithGeneralConstraint (Apply x))) = MkFunctorial (WithGeneralConstraint (Apply (fmap f x)))





-- {-# LANGUAGE Rank2Types #-}

example :: (forall a. [a] -> Int) -> Int
example f = f ([1,2,3] :: [Int]) + f "hallo welt!"





-- {-# LANGUAGE GADTs #-}

-- import Data.Map
-- import Data.IntMap

data GMap k v where
  GMap :: Map k v -> GMap k v
  IMap :: IntMap v -> GMap Int v

foo :: GMap String Int
foo = GMap $ Data.Map.fromList [("hallo", 5), ("welt", 4)]

baz :: GMap Int String
baz = IMap $ Data.IntMap.fromList [(5, "hallo"), (4, "welt")]

-- smartConstr :: (Typeable k, Ord k) => [(k, v)] -> GMap k v
-- smartConstr = ...

glookup :: Ord k => k -> GMap k v -> Maybe v
glookup k (GMap m) = Data.Map.lookup k m
glookup k (IMap m) = Data.IntMap.lookup k m


