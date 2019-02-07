module Ch6 where

import Prelude

import BigIntUtil (bigIntBitsToInt)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)
import Partial.Unsafe (unsafePartial)

-- make sure to run `npm i` to install big-integer js dependency

data RNG = RNG BigInt


mkRng :: Int -> RNG
mkRng = RNG <<< BigInt.fromInt

instance showRNG :: Show RNG where
  show (RNG n) = "RNG " <> (show n)

lcgC :: BigInt
lcgC = BigInt.fromInt 11

lcgM' :: BigInt
lcgM' = unsafeFromHex "FFFFFFFFFFFF"

lcgA :: BigInt
lcgA = unsafeFromHex "5DEECE66D"

nextInt :: RNG -> Tuple Int RNG
nextInt (RNG seed) =
  let
    newSeed = BigInt.and (seed * lcgA + lcgC) lcgM'
    nextRNG = RNG newSeed
    n = bigIntBitsToInt $ BigInt.shr newSeed 16.0
  in  Tuple n (RNG newSeed)

-- should only be used for constants
unsafeFromHex :: String -> BigInt
unsafeFromHex s = unsafePartial $ fromJust $ BigInt.fromBase 16 s


--

int :: RNG -> Tuple Int RNG
int = nextInt

randomPair :: RNG -> Tuple (Tuple Int Int) RNG
randomPair rng =
  let Tuple i1 rng2 = nextInt rng
      Tuple i2 rng3 = nextInt rng2
  in Tuple (Tuple i1 i2) rng3

nonNegativeInt :: RNG -> Tuple Int RNG
nonNegativeInt rng = 
  let 
    pair@(Tuple cand rng2) = nextInt rng
  in if cand >= 0
    then pair
    else nonNegativeInt rng2


number :: RNG -> Tuple Number RNG
number rng = 
  let
    Tuple i rng2 = nonNegativeInt rng
    d =  Int.toNumber i / Int.toNumber top 
  in Tuple d rng2

intNumber :: RNG -> Tuple ( Tuple Int Number ) RNG
intNumber rng =
  let 
    Tuple i rng2 = nextInt rng
    Tuple n rng3 = number rng2
  in Tuple (Tuple i n) rng3

numberInt :: RNG -> Tuple (Tuple Number Int ) RNG
numberInt rng =
  let 
    Tuple n rng2 = number rng
    Tuple i rng3 = nextInt rng2
  in Tuple (Tuple n i ) rng3

number3 :: RNG -> Tuple (Tuple3 Number Number Number) RNG
number3 rng =  
  let 
    Tuple n rng2 = number rng
    Tuple n2 rng3 = number rng2
    Tuple n3 rng4 = number rng3
  in Tuple (tuple3 n n2 n3) rng4

ints :: Int -> RNG -> Tuple (List Int) RNG
ints i rng | i <= 0 = Tuple Nil rng
          | otherwise = let 
              Tuple n rng2 = nextInt rng
              Tuple n2 rng3=  (ints (i - 1) rng2)
            in Tuple (Cons n n2) rng3    

type Rand a = RNG -> Tuple a RNG

unit :: forall a. a -> Rand a
unit = Tuple

mapRand :: forall a b. Rand a -> (a -> b) -> Rand b
mapRand rf f rng =
  let
    Tuple a rng2 = rf rng
  in Tuple (f a) rng2

number' :: RNG -> Tuple Number RNG
number' rng = mapRand int (\x -> Int.toNumber x / Int.toNumber top) rng

map2 :: forall a b c. Rand a -> Rand b -> (a -> b -> c) -> Rand c
map2 raf rbf f rng = 
  let
    Tuple a rng2 = raf rng
    Tuple b rng3 = rbf rng2
  in Tuple (f a b) rng3

both :: forall a b. Rand a -> Rand b -> Rand (Tuple a b)
both raf rbf = map2 raf rbf (\x y -> Tuple x y)

intNumber' :: Rand (Tuple Int Number)
intNumber' = both int number      

numberInt' :: Rand (Tuple Number Int)
numberInt' = both number int

sequence :: forall a. List (Rand a) -> Rand (List a)
sequence l = case l of
  Nil -> Tuple Nil
  Cons randh t -> map2 randh (sequence t) Cons
