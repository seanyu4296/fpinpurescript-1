module Ch5 where

import Prelude

import Chapter4 (List(..), Option(..), (:))
import Data.Foldable (class Foldable, foldr)
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype, unwrap)

newtype Stream a = Stream(Lazy (Step a))
data Step a = Empty | Conss a (Stream a)




derive instance newTypeStream :: Newtype (Stream a) _

toList :: forall a. Stream a -> List a
toList s = case force <<< unwrap $ s of
  Empty -> Nil
  Conss a y -> Cons a (toList y)


{- from :: Int -> Stream Int
from n = Stream (defer \_ -> Conss n (from n + 1))

take :: forall a. Int a -> Stream a -> Stream a
take 0 _ = Empty
take n s = Stream $ defer \_ -> case force <<< unwrap $ s of
  Empty -> Empty
  Conss h t -> Cons h (take (n - 1) t)

empty :: forall a. Stream a
empty = Stream $ defer \_ -> Nil

-}

appendStream :: forall a. Stream a -> Stream a -> Stream a
appendStream a b = Stream $ defer \_ -> case force <<< unwrap $ a of
  Empty -> force <<< unwrap $ b
  Conss h t -> Conss h (appendStream t b) 

bindStream :: forall a b. Stream a -> (a -> Stream b) -> Stream b
bindStream s f = case force <<< unwrap $ s of
  Empty -> Stream $ defer \_ -> Empty
  Conss h t -> appendStream (f h) (bindStream t f)


foldrStream :: forall a b. (a -> Lazy b -> Lazy b) -> Lazy b -> Stream a -> Lazy b
foldrStream f acc sa = bind (unwrap sa) (\s -> case s of
  Empty -> acc
  Conss h t -> f h (foldrStream f acc t)
) 

foldrStream' :: forall a b. (a -> Lazy b -> Lazy b) -> Lazy b -> Stream a -> Lazy b
foldrStream' f b sa =
  -- `>>=` is flatMap
   unwrap sa >>= \s -> case s of
    Empty -> b
    Conss a t -> f a (foldrStream f b t)