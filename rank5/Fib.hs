-- Problem: Product of consecutive Fib numbers
-- Rank: 5
-- Src: https://www.codewars.com/kata/5541f58a944b85ce6d00006a

module Fib where

-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci 
--   numbers.
productFib :: Integer -> (Integer, Integer, Bool)
productFib n = (a, b, if c == n then True else False)
               where (a, b, c) = head $ dropWhile (\(_, _, c) -> c < n) $ zip3 fibs (tail fibs) $ zipWith (*) fibs (tail fibs)
                     fibs      = 0 : 1 : zipWith (+) fibs (tail fibs)

