-- Problem: Snail
-- Rank: 4
-- Src: https://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1

module Snail where

strip :: [a] -> ([a], [a], [a])
strip ls = (start, mid, end)
           where (start, rest) = splitAt 1 ls
                 (mid, end)    = splitAt (size - 2) rest
                 size          = length ls 

snail :: [[Int]] -> [Int]
snail [] = []
snail [xs] = xs
snail ls = start ++ right ++ reverse end ++ reverse left ++ snail rest
           where ([start], mid, [end])         = strip ls
                 (left, rest, right)           = foldr stripAndCons ([], [], []) mid
                 stripAndCons ls (s, m, e)     = (x0 : s, xs : m, x1 : e)
                                                 where ([x0], xs, [x1]) = strip ls 
                 
snailBest :: [[Int]] -> [Int]
snailBest [] = []
snailBest (xs:xss) = xs ++ (snail . reverse . transpose) xss

