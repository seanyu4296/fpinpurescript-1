module Main where

import Chapter2

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log (formatResult "absolute value" (-42) abs)
  log (formatResult "factorial" 7 factorial)
  log (show (findFirst ["a", "b", "c"] "b"))
  log (show (polyFindFirst [1, 2, 3] (_ == 1)))
  log (show (isSorted [1, 2, 3, 3] (\a b -> a <= b)))
