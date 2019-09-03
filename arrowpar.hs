
module ArrowPar where


import qualified Control.Arrow as A
import qualified Control.Category as C
import qualified Control.Monad.Par as P
import qualified Control.Parallel.Strategies as S

import qualified Data.Functor.Contravariant as F

newtype ParF a b = ParF { f :: a -> S.Eval b }


{-
instance C.Category ParF where
  id  = ParF S.r0
  (ParF g) . (ParF f) = (ParF (f `S.dot` g))

instance Category ... where
  id  = id
  (.) = dot
-}




{-
 -    Trying thins out, might not be relatet to parallel arrows
 -}


-- Type representing a computation ...
data MyArr b c = MyArr (b -> (c, MyArr b c))

runMyArr :: MyArr b c -> b -> c
runMyArr (MyArr step) = fst . step


-- instance F.Contravariant (MyArr a) where
--   contramap f (MyArr g) = MyArr _

instance Functor (MyArr a) where
  fmap f (MyArr g) = MyArr (\e ->
    let r = g e
    in (f $ fst r, fmap f (snd r)))

instance Applicative (MyArr a) where
  pure a = MyArr (\_ -> (a, pure a))
  b@(MyArr f) <*> d@(MyArr g) = MyArr (\e ->
    let (a, _) = g e; (c, _) = f e
    in (c a, b <*> d) )

instance Monad (MyArr a) where
  (MyArr f) >>= g = MyArr (\e ->
    let (a, b) = f e; c = g a
    in (runMyArr c e, c))

instance C.Category MyArr where
  id = MyArr (\i -> (i, C.id))
  (MyArr f) . (MyArr g) = MyArr (\e ->
    let (a, b) = g e; (c, d) = f a
    in _)


-- -}
