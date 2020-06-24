-- Problem: Sudoku Solver
-- Rank: 3
-- Src: https://www.codewars.com/kata/5296bc77afba8baa690002d7

module Sudoku where

import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import Data.Array 

type Fuzzy a = [a]

certain :: Fuzzy a -> Bool
certain [_] = True
certain _   = False

type Array2D a = Array (Int, Int) a
type SudokuFuzzy = Map.Map (Int, Int) (Fuzzy Int)
type SudokuState b = State SudokuFuzzy b

constraint :: Array2D Int -> (Int, Int) -> Fuzzy Int
constraint arr (x, y)
  | arr ! (x, y) == 0 = [1..9] \\ (row ++ col ++ region)
  | otherwise         = [arr ! (x, y)] 
  where row    = [e | i <- [0..8], let e = arr ! (x, i), e /= 0]
        col    = [e | i <- [0..8], let e = arr ! (i, y), e /= 0]
        region = [e | i <- [m..m + 2], j <- [n..n + 2], let e = arr ! (i, j), e /= 0]
        m      = 3 * (x `div` 3)
        n      = 3 * (y `div` 3)

solvable :: SudokuFuzzy -> Bool
solvable s = if any null $ Map.elems s then False else True

solved :: SudokuFuzzy -> Bool
solved s = if all certain $ Map.elems s then True else False 

pickOne :: Array2D Int -> SudokuState (Maybe (Array2D Int))
pickOne arr  = do
  m <- get
  if solveable m 
  then
    let pos = fst . head . dropWhile $ ((/= 0) . snd) $ assocs m 
    x : xs <- Map.lookup pos m
    put $ Map.insert pos [x] m
    return Just $ accum (\_ -> \b -> b) arr [(pos, x)] 
  else return Nothing
    
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
