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
dropWhile Nil _ = Nil
dropWhile (x : Nil) f
  | f x = Nil
  | otherwise = x : Nil
dropWhile (x : xs) f
  | f x = dropWhile xs f
  | otherwise = x : xs

init :: forall a. List a -> List a
init Nil = Nil
init (_ : Nil) = Nil
init (h : t) = h : init t
