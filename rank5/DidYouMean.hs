-- Problem: Did you mean ...?
-- Rank: 5
-- Src: https://www.codewars.com/kata/5259510fc76e59579e0009d4
-- Note: general memorize

module DidYouMean where

import qualified Data.Map as M
import Control.Monad.State

minEditDistM :: Monad m => ((String, String) -> m Int) -> (String, String) -> m Int
minEditDistM f ([],     [])     = return 0
minEditDistM f (s,      [])     = return $ length s
minEditDistM f ([],     s)      = return $ length s
minEditDistM f ((x:xs), (y:ys)) | x == y    = f (xs, ys)
                                | otherwise = (+1) . minimum <$> (sequenceA $ f <$> [(xs, ys), (xs, (y:ys)), ((x:xs), ys)])

type StateMap a b = State (M.Map a b) b

memorizeM :: Ord a => ((a -> StateMap a b) -> (a -> StateMap a b)) -> a -> b
memorizeM t x = evalState (f x) M.empty where
  f x = get >>= \m -> maybe (g x) return (M.lookup x m)
  g x = do
        y <- t f x
        m <- get
        put $ M.insert x y m
        return y

minEditDist :: String -> String -> Int
minEditDist s1 s2 = memorizeM minEditDistM (s1, s2)

