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
foldRight l acc f = case l of
  Nil -> acc
  h : t -> f h (foldRight t acc f)

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

reverse :: forall a. List a -> List a
reverse l = foldLeft l Nil \acc x -> x : acc

append' :: forall a. List a -> List a -> List a
append' as bs = foldLeft (reverse as) bs \acc x -> x : acc

append'' :: forall a. List a -> List a -> List a
append'' as bs = foldRight as bs \x acc -> x : acc

concat :: forall a. List (List a) -> List a
concat l = foldLeft l Nil append'

addOneList :: List Int -> List Int
addOneList l = foldRight l Nil \x acc -> (x + 1) : acc

listToString :: List Number -> List String
listToString l = foldRight l Nil \x acc -> show x : acc

map' :: forall a b. List a -> (a -> b) -> List b
map' l f = foldLeft (reverse l) Nil \acc x -> f x : acc

filter :: forall a. List a -> (a -> Boolean) -> List a
filter l f = foldLeft (reverse l) Nil \acc x -> if f x then x : acc else acc

flatMap :: forall a b. List a -> (a -> List b) -> List b
flatMap l f = foldLeft (reverse l) Nil \acc x -> append' (f x) acc

filter' :: forall a. List a -> (a -> Boolean) -> List a
filter' l f = flatMap l \x -> if f x then (x : Nil) else Nil

addList :: List Int -> List Int -> List Int
addList (h : t) (h' : t') = (h + h') : addList t t'
addList _ _ = Nil

zipWith :: forall a b c. List a -> List b -> (a -> b -> c) ->  List c
zipWith (h : t) (h' : t') f = f h h' : zipWith t t' f
zipWith _ _ _ = Nil
