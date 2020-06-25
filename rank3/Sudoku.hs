-- Problem: Sudoku Solver
-- Rank: 3
-- Src: https://www.codewars.com/kata/5296bc77afba8baa690002d7

module Sudoku where

import Data.List
import Data.Array

type Array2D a = Array (Int, Int) a

constraint :: Array2D Int -> (Int, Int) -> [Int]
constraint arr (x, y)
  | arr ! (x, y) == 0 = [1..9] \\ (row ++ col ++ region)
  | otherwise         = [arr ! (x, y)] 
  where row    = [e | i <- [0..8], let e = arr ! (x, i), e /= 0]
        col    = [e | i <- [0..8], let e = arr ! (i, y), e /= 0]
        region = [e | i <- [m..m + 2], j <- [n..n + 2], let e = arr ! (i, j), e /= 0]
        m      = 3 * (x `div` 3)
        n      = 3 * (y `div` 3)

solved :: Array2D Int -> Bool
solved = all (/= 0) . elems

solvable :: Array2D Int -> Bool
solvable = any (== 0) . elems

findZero :: Array2D Int -> (Int, Int)
findZero arr = head . dropWhile ((/= 0) . (arr !)) $ indices arr

backtrack :: Array2D Int -> [Array2D Int]
backtrack arr | solved arr   = [arr]
              | solvable arr = concatMap backtrack $ map (f pos) xs
              | otherwise  = []
              where pos     = findZero arr
                    xs      = constraint arr pos
                    f pos x = accum (\_ -> \b -> b) arr [(pos, x)]

sudoku :: [[Int]] -> [[Int]]
sudoku puzzle = make2D 9 . elems $ head $ backtrack arr
                where arr            = listArray ((0, 0), (8, 8)) $ concat puzzle
                      make2D n ls    = if length ls == n
                                       then [ls]
                                       else first : make2D n rest
                                       where (first, rest) = splitAt n ls

