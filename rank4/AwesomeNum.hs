-- Problem: Catching Car Mileage Numbers
-- Rank: 4
-- Src: https://www.codewars.com/kata/52c4dd683bfd3b434c000292

module AwesomeNum where

import Data.Char (digitToInt)
import Data.List (nub, sort, sortBy)
import Data.Bifunctor (second)

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isTailZeros :: [Int] -> Bool
isTailZeros (_ : xs) = all (== 0) xs

isSameNum :: [Int] -> Bool
isSameNum (x : xs) = all (== x) xs

isSeq :: [Int] -> Bool
isSeq xs = isInc xs || isDec xs
  where isInc [_]            = True
        isInc (0 : xs)       = null xs
        isInc (x1 : x2 : xs) = if (x1 + 1) `mod` 10 == x2 then isInc (x2 : xs) else False
        isDec [_]            = True
        isDec (x1 : x2 : xs) = if x1 - 1 == x2 then isDec (x2 : xs) else False


isPalindrome :: [Int] -> Bool
isPalindrome xs = l == reverse r
  where (l, r) = if len `mod` 2 == 0
                 then halves
                 else second tail $ halves
        len    = length xs
        halves = splitAt (len `div` 2) xs

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
  | (length . show) x < 3    = No
  | any id $ pred <*> [x]  = Yes
  | any id $ pred <*> next = Almost
  | otherwise              = No
  where pred = (`elem` xs) : map (. (map digitToInt . show)) [isTailZeros, isSameNum, isSeq, isPalindrome]
        next = [(x + 1), (x + 2)]

