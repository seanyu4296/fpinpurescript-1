module Chapter2
  (formatAbs) where

import Prelude

abs :: Int -> Int
abs n = if n < 0 then -n else n

formatAbs :: Int -> String
formatAbs x = "The absolute value of " <> show x <> " is " <> show (abs x)
