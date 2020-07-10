-- Problem: Smallest possible sum
-- Rank: 4
-- Src: https://www.codewars.com/kata/52f677797c461daaf7000740
-- Note: the key idea is that it actually is the process of Euclidean algorithm
-- gcd a b | a == b    = a
--         | a > b     = gcd b (a - b)
--         | otherwise = gcd a (b - a)

module SmallestPossibleSum where

smallestPossibleSum :: (Integral a) => [a] -> a
smallestPossibleSum xs = foldr1 gcd xs * fromIntegral (length xs)

