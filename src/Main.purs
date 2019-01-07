module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

abs :: Int -> Int
abs n = if n < 0 then -n else n

formatAbs :: Int -> String
formatAbs x = "The absolute value of " <> show x <> " is " <> show (abs x)

main :: Effect Unit
main = do
  log (formatAbs (-42))
