module Chapter3 where

import Prelude

data List a = Nil | Cons a (List a)

instance showList :: Show a => Show (List a) where
  show Nil = "Nil"
  show (Cons x Nil) = show x
  show (Cons x xs) = show x <> ", " <> show xs

sum :: List Int -> Int
sum Nil = 0
sum (Cons x xs) = x + sum xs

product :: List Number -> Number
product Nil = 1.0
product (Cons 0.0 _) = 0.0
product (Cons x xs) = x * product xs

infixr 8 Cons as :

tail :: forall a. List a -> List a
tail Nil = Nil
tail (Cons x xs) = xs

setHead :: forall a. a -> List a -> List a
setHead _ Nil = Nil
setHead a (x : xs) = a : xs

drop :: forall a. List a -> Int -> List a
drop Nil _ = Nil
drop (x : Nil) n
  | n > 0 = Nil
  | otherwise = x : Nil
drop (x : xs) n
  | n > 0 = drop xs (n - 1)
  | otherwise = x : xs

dropWhile :: forall a. List a -> (a -> Boolean) -> List a
dropWhile l f = case l of
  h : t | f h -> dropWhile t f
  _ -> l

init :: forall a. List a -> List a
init Nil = Nil
init (_ : Nil) = Nil
init (h : t) = h : init t

foldRight :: forall a b. List a -> b -> (a -> b -> b) -> b
foldRight as z f = case as of
  Nil -> z
  h : t -> f h (foldRight t z f)

sum' :: List Int -> Int
sum' ns = foldRight ns 0 (+)

product' :: List Number -> Number
product' ns = foldRight ns 1.0 (*)

length' :: forall a. List a -> Int
length' l = foldRight l 0 \_ y -> y + 1

foldLeft :: forall a b. List a -> b -> (b -> a -> b) -> b
foldLeft l z f = case l of
  Nil -> z
  h : t -> foldLeft t (f z h) f

sum'' :: List Int -> Int
sum'' l = foldLeft l 0 (+)

product'' :: List Number -> Number
product'' l = foldLeft l 1.0 (*)

length'' :: forall a. List a -> Int
length'' l = foldLeft l 0 \x y -> x + 1
