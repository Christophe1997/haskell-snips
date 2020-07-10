-- Problem: Hamming Numbers
-- Rank: 4
-- Src: https://www.codewars.com/kata/526d84b98f428f14a60008da

module Hamming where

hamming  :: Int -> Int
hamming = ((0 : 1 : [x | x <- [2..], isHamming x]) !!)
  where isHamming x
          | x == 1         = True
          | x `mod` 5 == 0 = isHamming $ x `div` 5
          | x `mod` 3 == 0 = isHamming $ x `div` 3
          | x `mod` 2 == 0 = isHamming $ x `div` 2
          | otherwise      = False

-- A better solution
-- hamming  :: Int -> Int
-- hamming n = seq !! (n - 1)
--   where seq = 1 : map (* 2) seq `join` map (* 3) seq `join` map (* 5) seq
--         join xxs@(x : xs) yys@(y : ys)
--           | x > y     = y : join xxs ys
--           | x < y     = x : join xs yys
--           | otherwise = x : join xs ys
