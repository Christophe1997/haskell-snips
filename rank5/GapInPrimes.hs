-- Problem: Gap in Primes
-- Rank: 5
-- Scr: https://www.codewars.com/kata/561e9c843a2ef5a40c0000a4

module GapInPrimes where

import Data.List(find)

gap :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
gap g m n = find (\(a, b) -> b - a == g) $ zip primes (tail primes)
            where primes    = filter isPrime [m..n]
                  isPrime n = all (\d -> n `mod` d /= 0) [2 .. floor . sqrt . fromIntegral $ n]

