module Main where

import Chapter5
import Data.Lazy
import Data.Tuple
import Prelude

import Async (runAsync, x)
import Chapter4 (List(Nil), (:), Option(..))
import Debug.Trace (spy, trace)
import Effect (Effect)
import Effect.Console (log, logShow)

main :: Effect Unit
main = do
  logShow $ toList $ take (constant' 10) 5
  runAsync x
  -- logShow $ toList $ take (from 10) 5
  -- logShow $ toList $ take iStream 5
  -- logShow $ toList $ take squareds 30
  -- logShow $ toList $ take (from' 10) 10
  -- logShow $ toList $ take (fibs') 10
  -- logShow $ toList $ fromList (1 : 2 : 3 : 4 : 5 : Nil)
  -- logShow $ toList $ take (fromList (1 : 2 : 3 : 4 : 5 : Nil)) 3
  -- logShow $ toList $ drop (fromList (1 : 2 : 3 : 4 : 5 : Nil)) 3
  -- logShow $ toList $ takeWhile (fromList (1 : 2 : 3 : 4 : 5 : Nil)) (_ < 3)
  -- logShow $ toList $ takeWhile' (fromList (1 : 2 : 3 : 4 : 5 : Nil)) (_ < 4)
  -- logShow $ forAll (fromList (1 : 2 : 3 : 4 : 5 : Nil)) (_ < 7)
  -- logShow $ toList $ take ones 10
  -- logShow $ exists ones (_ == 1)
  -- logShow $ toList $ takeWhile' iStream (_ < 5)
  -- log "done"
  -- logShow $ headOption' iStream
  -- logShow $ toList $ take (mapStream ones (_ + 1)) 10
  -- logShow $ toList $ take (filterStream (drop iStream 5) (\x -> x `mod` 2 == 0)) 10
  -- logShow $ toList $ take (appendStream iStream iStream) 10
  -- logShow $ toList $ take (flatMapStream' (drop iStream 5) (\x -> fromList (x : x : Nil))) 10

  -- logShow $ toList' (defer (\y -> SEmpty ))
--  logShow $ toList' (Lazy (SCons "a" (Lazy (SCons "b" (Lazy (SCons "c" (Lazy SEmpty)))))))
  -- logShow $ toList $ take (SCons (\_ -> 1) (\_ -> SCons (\_ -> 2) (\_ -> SEmpty))) 2

  -- where
  --   two :: Unit -> Int
  --   two _ = spy "two" 2

    -- three :: Unit -> Int
    -- three _ = spy "three" 3

    -- ss :: Stream Int
    -- ss = fromList' $ two : three : Nil

    -- xx = toList $ take ss 2


-- add2 :: Int -> Int
-- add2 x = x + 2