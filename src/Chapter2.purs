module Chapter2 where

import Prelude
import Data.Array
import Data.Maybe
import Data.Tuple

abs :: Int -> Int
abs n = if n < 0 then -n else n

formatAbs :: Int -> String
formatAbs x = "The absolute value of " <> show x <> " is " <> show (abs x)

factorial :: Int -> Int
factorial n = go n 1
  where
    go :: Int -> Int -> Int
    go x acc | x <= 0 = acc
             | otherwise = go (x-1) (x*acc)

formatResult :: String -> Int -> (Int -> Int) -> String
formatResult name n f = "The " <> name <> " of " <> show n <> " is " <> show (f n)

findFirst ::  Array String -> String -> Int
findFirst ss key = loop(0)
  where
    loop :: Int -> Int
    loop n | n >= length ss = -1
           | index ss n == Just key = n
           | otherwise = loop (n + 1)

polyFindFirst :: forall a. Array a -> (a -> Boolean) -> Int
polyFindFirst as p = loop(0)
  where
    compare :: forall a. (a -> Boolean) -> Maybe a -> Boolean
    compare f b = case b of
      Just x -> f x
      _ -> false
    loop :: Int -> Int
    loop n | n >= length as = -1
           | compare p (index as n) = n
           | otherwise = loop (n + 1)

isSorted :: forall a. Array a -> (a -> a -> Boolean) -> Boolean
isSorted as ordered = loop(0)
  where
    compare :: forall a. (a -> a -> Boolean) -> Maybe a -> Maybe a -> Boolean
    compare f (Just b) (Just c) = f b c
    compare f (Just b) Nothing = true
    compare _ _ _ = true
    loop :: Int -> Boolean
    loop n | n >= length as = true
           | compare ordered (index as n) (index as (n + 1)) = loop (n + 1)
           | otherwise = false

curry :: forall a b c. (Tuple a b -> c) -> (a -> (b -> c))
curry f x y = f (Tuple x y)
