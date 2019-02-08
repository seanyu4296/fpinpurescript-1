module Ch6 where

import Prelude

import BigIntUtil (bigIntBitsToInt)
import Data.BigInt (BigInt, and)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
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

int :: Rand Int
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

unitR :: forall a. a -> Rand a
unitR = Tuple

mapRand :: forall a b. (RNG -> Tuple a RNG) -> (a -> b) -> RNG -> Tuple b RNG
mapRand rf f rng =
  let
    Tuple a rng2 = rf rng
  in Tuple (f a) rng2


{- mapRand' :: forall a b. (Rand a) -> (a -> b) -> (Rand b)
mapRand' raf f rng=
  let
    Tuple a rng2 = raf rng
  in Tuple f a rbng2 -}

number'':: RNG -> Tuple Number RNG
number'' = mapRand nonNegativeInt (\x -> Int.toNumber x / Int.toNumber top)


number' :: RNG -> Tuple Number RNG
number' rng = mapRand int (\x -> Int.toNumber x / Int.toNumber top) rng

map2 :: forall a b c. Rand a -> Rand b -> (a -> b -> c) -> Rand c
map2 raf rbf f rng = 
  let
    Tuple a rng2 = raf rng
    Tuple b rng3 = rbf rng2
  in Tuple (f a b) rng3

{- map2' :: forall a b c. Rand a -> Rand b -> (a -> b -> c ) -> Rand c
map2' raf rbf f rng =
  let 
    Tuple a rng2 = raf rng
    Tuple b rng3 = rbf rng2
  in Tuple (f a b) rng3 -}

both' :: forall a b. Rand a -> Rand b -> Rand (Tuple a b)
both' raf rbf = map2 raf rbf (\x y -> Tuple x y)

both :: forall a b. Rand a -> Rand b -> Rand (Tuple a b)
both raf rbf = map2 raf rbf (\x y -> Tuple x y)

intNumber' :: Rand (Tuple Int Number)
intNumber' = both int number      

numberInt' :: Rand (Tuple Number Int)
numberInt' = both number int


sequence :: forall a. List (Rand a ) -> Rand (List a)
sequence l rng = case l of
  Nil -> Tuple Nil rng
  Cons rhf t -> (map2 rhf (sequence t) Cons) rng

flatMap :: forall a b. Rand a -> (a -> Rand b) -> Rand b
flatMap raf f rng = 
  let 
    Tuple a rng2 = raf rng
  in (f a) rng2

{- 
sequence :: forall a. List (Rand a) -> Rand (List a)
sequence l = case l of
  Nil -> Tuple Nil
  Cons randh t -> map2 randh (sequence t) Cons -}

map'':: forall a b. Rand a -> (a -> b) -> Rand b
map'' raf f = flatMap raf \x -> unitR (f x)

map2'':: forall a b c. Rand a -> Rand b -> (a -> b -> c ) -> Rand c
map2'' raf rbf f = 
  flatMap raf (\a -> 
    flatMap rbf (\b -> 
      unitR $ f a b
    )
  )


newtype State s a = State (s -> Tuple a s)

derive instance newTypeState :: Newtype (State s a) _

runState :: forall s a. State s a -> s -> Tuple a s
runState (State f) = f

type Rand' a = State BigInt a

nextInt' :: Rand' Int
nextInt' = State $ \s -> 
  let
    newSeed :: BigInt
    newSeed = BigInt.and (s * lcgA + lcgC) lcgM'
    n :: Int
    n = bigIntBitsToInt $ BigInt.shr newSeed 16.0
  in Tuple n newSeed

int' :: Rand' Int
int' = nextInt'
{- int :: Rand' Int
int = nextInt -}

unitState :: forall s a. a -> State s a
unitState a = State \s -> Tuple a s

mapS :: forall a b s. (State s a) -> (a -> b) -> (State s b)
mapS sa f = State $ \s -> 
  let
    Tuple a s2 = unwrap sa $ s
  in Tuple (f a) s2

mapS2 :: forall a b c s. State s a -> State s b -> (a -> b -> c) -> State s c
mapS2 sa sb f = State $ \s ->
  let
    Tuple a s2 = unwrap sa $ s
    Tuple b s3 = unwrap sb $ s2
  in Tuple (f a b) s3

applyS :: forall s a b. State s a -> State s (a -> b) -> State s b
applyS sa sbf = mapS2 sa sbf (\a bf -> bf a )

applyS' :: forall s a b. State s a -> State s (a -> b) -> State s b
applyS' sa sbf = State $ \s -> 
  let 
    Tuple a s2 = unwrap  sa $ s
    Tuple bf s3 = unwrap  sbf $ s2
  in Tuple (bf a) s2

{- f but partially applied by something -}
{- mapS2'' :: forall a b c s. State s a -> State s b -> (a -> b -> c) -> State s c
mapS2'' sa sb f = applyS' sa (State $ \s -> 
  let
    Tuple a s2 = unwrap sa $ s
  in Tuple (f a) s2
) -}

mapS2'' :: forall a b c s. State s a -> State s b -> (a -> b -> c) -> State s c
mapS2'' sa sb f = 
  applyS' sa ( State $ \s ->
    applyS' sb ( State $ \s2 -> 
      f s s2 
    ) 
  )


flatMapS:: forall a b s. (State s a) -> (a -> State s b) -> State s b
flatMapS (State saf) f = State $ \s ->
  let
    Tuple a s2 = saf s
  in unwrap (f a) $ s2


mapS' :: forall a b s. (State s a) -> (a -> b) -> (State s b)
mapS' sa f = flatMapS sa \a -> unitState $ f a

mapS2' :: forall a b c s. State s a -> State s b -> (a -> b -> c ) -> State s c
mapS2' sa sb f = 
  flatMapS sa (\a -> 
    flatMapS sb (\b ->
      unitState $ f a b
    )
  )

sequenceS :: forall a s. List (State s a) -> State s (List a)
sequenceS Nil = unitState $ Nil
sequenceS (Cons sh t) = mapS2' sh (sequenceS t) Cons

instance functorState :: Functor (State s) where
  map f sa = State $ \s -> 
    let
      Tuple a s2 = unwrap sa $ s
    in Tuple (f a) s2

instance applyState :: Apply (State s) where
  apply sb f sa = State $ \s ->

instance applicativeState :: Applicative (State s) where
  pure a = State \s -> Tuple a s -}

ns :: Rand' (List Int)
ns = flatMapS int' (\x ->
  flatMapS int' (\y -> 
    unitState $ Cons x (Cons y Nil)
  )
)


get :: forall s. State s s
get = State \s -> Tuple s s

set :: forall s. s -> State s Unit
set s = State \_ -> Tuple unit s



data Input = Coin | Turn
type Machine = {
  locked :: Boolean,
  candies :: Int,
  coins :: Int
}

type MachineState a = State Machine a

y :: Rand' Int
y = map (\x -> x + 1) int' 

-- runInput :: Input -> MachineState Unit
