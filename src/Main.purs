module Main where

import Chapter2
import Chapter3
import Prelude

import Effect (Effect)
import Effect.Console (log)

sampleList :: List Int
sampleList = (1 : 2 : 3 : 4 : 5 : Nil)

sampleListNum :: List Number
sampleListNum = (1.0 : 2.0 : 3.0 : Nil)

main :: Effect Unit
main = do
  log $ show $ sum sampleList
  log $ show $ tail sampleList
  log $ show sampleList
  log $ show $ setHead 5 sampleList
  log $ show $ drop sampleList 3
  log $ show $ dropWhile sampleList (_ < 3)
  log $ show $ init sampleList
  log $ show $ sum' sampleList
  log $ show $ product' sampleListNum
  log $ show $ length' sampleList
  log $ show $ sum'' sampleList
  log $ show $ product'' sampleListNum
  log $ show $ length'' sampleList
