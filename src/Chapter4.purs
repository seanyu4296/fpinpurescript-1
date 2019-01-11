module Chapter4(
  List(Nil, Cons),
  (:),
  Option(..)
) where

import Prelude

import Data.Int

instance showOption :: Show a => Show (Option a) where
  show None = "None"
  show (Some x) = "Some(" <> show x <> ")"

instance showList :: Show a => Show (List a) where
  show Nil = "Nil"
  show (Cons x xs) = show x <> " : " <> show xs

instance showEither :: (Show a, Show e) => Show (Either e a) where
  show (Right a) = "Right(" <> show a <> ")"
  show (Left e) = "Left(" <> show e <> ")"

data List a = Nil | Cons a (List a)

infixr 8 Cons as :

foldLeft :: forall a b. List a -> b -> (b -> a -> b) -> b
foldLeft l z f = case l of
  Nil -> z
  Cons h t -> foldLeft t (f z h) f

foldRight :: forall a b. List a -> b -> (a -> b -> b) -> b
foldRight l acc f = case l of
  Nil -> acc
  h : t -> f h (foldRight t acc f)

sum :: List Number -> Number
sum l = foldLeft l 0.0 (+)

length' :: List Number -> Int
length' l = foldLeft l 0 \acc x -> acc + 1

reverse :: forall a. List a -> List a
reverse l = foldLeft l Nil \acc x -> Cons x acc

map' :: forall a b. List a -> (a -> b) -> List b
map' l f = foldLeft (reverse l) Nil \acc x -> Cons (f x) acc

data Option a = Some a | None

map :: forall a b. Option a -> (a -> b) -> Option b
map x f = case x of
  Some m -> Some (f m)
  None -> None

flatMap :: forall a b. Option a -> (a -> Option b) -> Option b
flatMap x f = case x of
  Some m -> f m
  None -> None

getOrElse :: forall a. Option a -> a -> a
getOrElse x y = case x of
  Some m -> m
  None -> y

orElse :: forall a. Option a -> Option a -> Option a
orElse x y = case x of
  Some m -> Some m
  None -> y

filter :: forall a. Option a -> (a -> Boolean) -> Option a
filter s@(Some m) f | f m = s
filter _          f       = None

mean :: List Number -> Number
mean Nil = 0.0
mean l = sum l / toNumber (length' l)

variance :: List Number -> Option Number
variance Nil = None
variance l = Some (sum (map' l (\x -> (x - mean l) * (x - mean l))) / toNumber (length' l))

map2 :: forall a b c. Option a -> Option b -> (a -> b -> c) -> Option c
map2 (Some x) (Some y) f = Some (f x y)
map2 _ _ _ = None

sequence :: forall a. List (Option a) -> Option (List a)
sequence Nil = None
sequence l = foldRight l (Some Nil) (\x acc -> map2 x acc (\y z -> y : z))

traverse :: forall a b. List a -> (a -> Option b) -> Option (List b)
traverse Nil _ = None
traverse l f = foldRight l (Some Nil) (\x acc -> map2 (f x) acc (\y z -> y : z))

--

data Either e a = Left e | Right a

eitherMap :: forall e a b. Either e a -> (a -> b) -> Either e b
eitherMap x f = case x of
  Right a -> Right (f a)
  Left e -> Left e

eitherFlatMap :: forall e a b. Either e a -> (a -> Either e b) -> Either e b
eitherFlatMap x f = case x of
  Right a -> f a
  Left e -> Left e

eitherOrElse :: forall e a. Either e a -> Either e a -> Either e a
eitherOrElse (Right a) _ = Right a
eitherOrElse (Left e) x = x

eitherMap2 :: forall e a b c. Either e a -> Either e b -> (a -> b -> c) -> Either e c
eitherMap2 (Right a) (Right b) f = Right (f a b)
eitherMap2 _ (Left e) _ = Left e
eitherMap2 (Left e) _ _ = Left e

eitherSequence :: forall e a. List (Either e a) -> Either e (List a)
eitherSequence l = foldRight l (Right Nil) (\x acc -> eitherMap2 acc x (\acc' x' -> x' : acc'))

eitherTraverse :: forall e a b. List a -> (a -> Either e b) -> Either e (List b)
eitherTraverse l f = foldRight l (Right Nil) (\x acc -> eitherMap2 acc (f x) (\acc' x' -> x' : acc'))
