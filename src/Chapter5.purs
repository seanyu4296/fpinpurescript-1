module Chapter5 where

import Prelude

import Chapter4 (List(Nil), (:), Option(..))
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple

newtype Stream a = Stream (Lazy (Step a))
data Step a = Empty | Cons a (Stream a)

derive instance newTypeStream :: Newtype (Stream a) _

infixr 8 Cons as ~

sEmpty :: forall a. Stream a
sEmpty = Stream $ defer \_ -> Empty

toList :: forall a. Stream a -> List a
toList s = case force $ unwrap $ s of
  Empty -> Nil
  h ~ t -> h : (toList (t))

fromList :: forall a. List a -> Stream a
fromList (h : t) = Stream $ defer \_ -> (h ~ (fromList t))
fromList _ = Stream $ defer \_ -> Empty

fromList' :: forall a. List (Unit -> a) -> Stream a
fromList' (h : t) = Stream $ defer \_ -> (h unit ~ (fromList' t))
fromList' _ = Stream $ defer \_ -> Empty

take :: forall a. Stream a -> Int -> Stream a
take (Stream s) n = Stream $ defer \_ -> case force s of
  h ~ t
    | n <= 0 -> Empty
    | otherwise -> (h ~ take t (n - 1))
  _ -> Empty

drop :: forall a. Stream a -> Int -> Stream a
drop (Stream s) n = Stream $ defer \_ -> case force s of
  h ~ t
    | n <= 0 -> (h ~ t)
    | otherwise -> force $ unwrap $ drop t (n - 1)
  _ -> Empty

takeWhile :: forall a. Stream a -> (a -> Boolean) -> Stream a
takeWhile (Stream s) f = Stream $ case force s of
  h ~ t
    | f h -> defer \_ -> (h ~ (takeWhile t f))
    | otherwise -> defer \_ -> Empty
  _ -> defer \_ -> Empty

-- foldRight :: forall a b. Stream a -> b -> (a -> b -> b) -> b
-- foldRight (Stream s) b f = case force s of
--   h ~ t -> f h (foldRight t b f)
--   _ -> b

foldRight :: forall a b. Stream a -> Lazy b -> (a -> Lazy b -> b) -> b
foldRight (Stream s) b f = case force s of
  h ~ t -> f h (defer \_ -> (foldRight t b f))
  _ -> force b

-- foldRight' :: forall a b. Stream a -> Lazy b -> (a -> Lazy b -> Lazy b) -> Lazy b
-- foldRight' (Stream s) b f = defer \_ -> case force s of
--   h ~ t -> f h (defer \_ -> (foldRight t b f))
--   _ -> force b

-- foldRight'' :: forall a b. Stream a -> b -> (a -> b -> b) -> b
-- foldRight'' s b f = foldRight' s (defer \_ -> b) (\x acc -> f x (force acc))

forAll :: forall a. Stream a -> (a -> Boolean) -> Boolean
forAll s f = foldRight s (defer \_ -> true) (\x acc -> f x && force acc)

takeWhile' :: forall a. Stream a -> (a -> Boolean) -> Stream a
takeWhile' s f = foldRight s (defer\_ -> Stream $ defer \_ -> Empty) (\x acc ->
   if f x then Stream $ defer \_ -> Cons x (force acc) else Stream $ defer \_ -> Empty)

ones :: Stream Int
ones = Stream $ defer \_ -> Cons 1 ones

exists :: forall a. Stream a -> (a -> Boolean) -> Boolean
exists s f = foldRight s (defer \_ -> false) (\x acc -> f x || force acc)

iStream :: Stream Int
iStream = loop 1
  where
    loop :: Int -> Stream Int
    loop n = Stream $ defer \_ -> n ~ (loop (n + 1))

from :: Int -> Stream Int
from n = loop n
  where 
    loop :: Int -> Stream Int
    loop m = Stream $ defer \_ -> m ~ (loop (m + 1) )    

constant :: forall a. a -> Stream a
constant x = loop x
  where
    loop :: a -> Stream a
    loop n = Stream $ defer \_ -> n ~ (loop (n))

squareds :: Stream Int
squareds = Stream $ (defer (\_ -> Cons 0 (loop 1)))
  where
    loop :: Int -> Stream Int
    loop n = Stream $ defer (\_ -> Cons n (loop (n + n)))

fibs :: Stream Int
fibs =  loop 0 1
  where 
    loop :: Int -> Int -> Stream Int
    loop m n = Stream $ defer (\_ -> Cons m (loop n (m +n)))
{- 
fibs :: Stream Int
fibs = (fromList $ 0 : 1 : Nil) `append` tl
  where tl = zipWith fibs tl (+) -}

unfold :: forall a s. s -> (s -> Option (Tuple a s)) -> Stream a
unfold s f = Stream $ (defer (\_ -> case f s of
  Some (Tuple a ns) -> Cons a (unfold ns f)
  _ -> Empty
))

constant' :: forall a. a -> Stream a
constant' x = unfold x (\s -> Some(Tuple s s))

from' :: Int -> Stream Int
from' n = unfold n (\s -> Some(Tuple s (s + 1)))


fibs' :: Stream Int
fibs' = unfold (Tuple 0 1) (\s -> Some (Tuple (fst s) (Tuple (snd s) ((fst s) + (snd s)))))

{- fibs' :: Stream Int
fibs' = loop 0 1
  where 
    loop :: Int -> Int -> Stream Int
    loop m n = unfold m (\s -> Some(Tuple (s +) (s + n))) -}

headOption :: forall a. Stream a -> Option a
headOption (Stream a) = case force a of
  Empty -> None
  h ~ t -> Some h

headOption' :: forall a. Stream a -> Option a
headOption' s = foldRight s (defer \_ -> None) (\x acc -> Some x)

mapStream :: forall a b. Stream a -> (a -> b) -> Stream b
mapStream s f = foldRight s (defer \_ -> sEmpty) (\x acc -> Stream $ defer \_ -> f x ~ force acc)

filterStream :: forall a. Stream a -> (a -> Boolean) -> Stream a
filterStream s f = foldRight s (defer \_ -> sEmpty) (\x acc ->
  if f x then Stream $ defer \_ -> x ~ (force acc) else force acc
)

appendStream :: forall a. Stream a -> Stream a -> Stream a
appendStream s s' = foldRight s (defer \_ -> s') (\x acc ->
  Stream $ defer \_ -> x ~ force acc)

flatMapStream :: forall a b. Stream a -> (a -> Stream b) -> Stream b
flatMapStream s f = foldRight s (defer \_ -> sEmpty) (\x acc ->
  appendStream (f x) (force acc)
)

flatMapStream' :: forall a b. Stream a -> (a -> Stream b) -> Stream b
flatMapStream' s f = foldRight s (defer \_ -> sEmpty) (\x acc ->
  case force $ unwrap (f x) of
    h ~ t -> Stream $ defer \_ -> h ~ (go acc)
    _ -> sEmpty
)
  where
    go :: Lazy (Stream b) -> Stream b
    go y = case force y of
      Stream z -> case force z of
        h' ~ t' -> Stream $ defer \_ -> h' ~ go (defer \_ -> t')
        _ -> sEmpty
