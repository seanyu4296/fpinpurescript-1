module Main where

import Chapter2
import Chapter3
import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

sampleList :: List Int
sampleList = (1 : 3 : 3 : 4 : 3 : 2 : 5 : Nil)

sampleList' :: List Int
sampleList' = (6 : 7 : 8 : 9 : 10 : Nil)

sampleListNum :: List Number
sampleListNum = (1.0 : 2.0 : 3.0 : Nil)

sampleTree :: Tree Int
sampleTree = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Branch (Leaf 4) (Leaf 5)))

main :: Effect Unit
main = do
  logShow $ sum sampleList
  logShow $ tail sampleList
  logShow sampleList
  logShow $ setHead 5 sampleList
  logShow $ drop sampleList 3
  logShow $ dropWhile sampleList (_ < 3)
  logShow $ init sampleList
  logShow $ sum' sampleList
  logShow $ product' sampleListNum
  logShow $ length' sampleList
  logShow $ sum'' sampleList
  logShow $ product'' sampleListNum
  logShow $ length'' sampleList
  logShow $ reverse sampleList
  logShow $ append' sampleList (6 : 7 : 8 : Nil)
  logShow $ append'' sampleList (6 : 7 : 8 : Nil)
  logShow $ concat (sampleList : sampleList' : Nil)
  logShow $ addOneList sampleList
  logShow $ listToString sampleListNum
  logShow $ map' sampleList \x -> x + 1
  logShow $ filter sampleList \x -> (mod x 2) == 0
  logShow $ flatMap sampleList \x -> (x : x : Nil)
  logShow $ filter' sampleList \x -> (mod x 2) == 0
  logShow $ addList sampleList sampleList'
  logShow $ zipWith sampleList sampleList' (*)
  logShow $ hasSubsequence sampleList (1 : 2 : Nil)
  logShow $ sampleTree
  logShow $ treeSize sampleTree
  logShow $ treeMax sampleTree
  logShow $ treeDepth sampleTree
  logShow $ treeMap sampleTree \x -> x + 1
  logShow $ treeSize' sampleTree
  logShow $ treeMax' sampleTree
  logShow $ treeDepth' sampleTree
  logShow $ treeMap' sampleTree \x -> x + 1
