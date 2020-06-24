-- Problem: Can you get the loop
-- Rank: 5
-- Src: https://www.codewars.com/kata/52a89c2ea8ddc5547a000863

module CanYouGetTheLoop where
import CanYouGetTheLoop.Types
import Data.List(elemIndex)

{-
data Node a
instance Eq a => Eq (Node a)

next :: Node a -> Node a
-}

loopSize :: Eq a => Node a -> Int
loopSize start = 
  let aux cur acc = if cur `elem` acc then cur `elemIndex` acc else aux (next cur) (cur:acc)
  in maybe undefined (+ 1) $ aux start []

