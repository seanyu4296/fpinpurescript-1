module Main where

import Chapter2 (formatAbs)

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log (formatAbs (-42))
