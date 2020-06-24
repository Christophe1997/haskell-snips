-- Problem: Sudoku Solver
-- Rank: 3
-- Src: https://www.codewars.com/kata/5296bc77afba8baa690002d7

module Sudoku where

import Data.List
import Data.Array

type Fuzzy a = [a]

certain :: Fuzzy a -> Bool
certain [_] = True
certain _   = False

type Array2D a = Array (Int, Int) a
type SudokuFuzzy = [((Int, Int), Fuzzy Int)]

constraint :: Array2D Int -> (Int, Int) -> Fuzzy Int
constraint arr (x, y)
  | arr ! (x, y) == 0 = [1..9] \\ (row ++ col ++ region)
  | otherwise         = [arr ! (x, y)] 
  where row    = [e | i <- [0..8], let e = arr ! (x, i), e /= 0]
        col    = [e | i <- [0..8], let e = arr ! (i, y), e /= 0]
        region = [e | i <- [m..m + 2], j <- [n..n + 2], let e = arr ! (i, j), e /= 0]
        m      = 3 * (x `div` 3)
        n      = 3 * (y `div` 3)

genConstraint :: Array2D Int -> SudokuFuzzy
genConstraint arr = (\e -> (e, constraint arr e)) <$> indices arr

pickOne :: SudokuFuzzy -> ((Int, Int), Fuzzy Int)
pickOne = head . dropWhile (certain . snd)

solvable :: SudokuFuzzy -> Bool
solvable c = if any (null . snd) c then False else True

solved :: SudokuFuzzy -> Bool
solved c = if all (certain . snd) c then True else False 

update :: SudokuFuzzy -> Array2D Int -> Array2D Int
update c arr = accum (\_ -> \b -> b) arr $ map (\(e, [x]) -> (e, x)) $ filter (certain . snd) c

guess :: Array2D Int -> ((Int, Int), Fuzzy Int) -> Maybe (Array2D Int)
guess arr (pos, [])     = Nothing
guess arr (pos, x : xs) = if solvable updatedC
                          then if solved updatedC
                               then Just updatedArr
                               else case guess updatedArr (pickOne $ updatedC) of
                                    Nothing -> guess arr (pos, xs)
                                    e       -> e
                          else guess arr (pos, xs)
                          where c          = genConstraint newArr
                                newArr     = accum (\_ -> \b -> b) arr [(pos, x)]
                                updatedArr = update c newArr
                                updatedC   = genConstraint updatedArr

sudoku :: [[Int]] -> [[Int]]
sudoku puzzle = make2D 9 . elems $ maybe undefined id (guess updatedArr $ pickOne updatedC)
                where arr            = listArray ((0, 0), (8, 8)) $ concat puzzle
                      c              = genConstraint arr
                      updatedArr     = update c arr
                      updatedC       = genConstraint updatedArr
                      make2D n ls    = if length ls == n
                                       then [ls]
                                       else first : make2D n rest
                                       where (first, rest) = splitAt n ls

puzzle :: [[Int]]
puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]
