-- Problem: My smallest code interpreter (aka Brainf**k) 
-- Rank: 5
-- Src: https://www.codewars.com/kata/526156943dfe7ce06200063e

module Brainfuck where

import Data.Word
import Data.Char

interpret :: String -> String -> String -> [Word8] -> ([Word8], [Word8]) -> String
interpret [] stack input output env                     = reverse $ (chr . fromIntegral) <$> output
interpret ('>' : xs) stack input output (ys, [z])       = interpret xs ('>' : stack) input output (z : ys, [0])
interpret ('>' : xs) stack input output (ys, z : zs)    = interpret xs ('>' : stack) input output (z : ys, zs)
interpret ('<' : xs) stack input output (y : ys, zs)    = interpret xs ('<' : stack) input output (ys, y : zs)
interpret ('+' : xs) stack input output (ys, z : zs)    = interpret xs ('+' : stack) input output (ys, (z + 1) : zs)
interpret ('-' : xs) stack input output (ys, z : zs)    = interpret xs ('-' : stack) input output (ys, (z - 1) : zs)
interpret ('.' : xs) stack input output (ys, z : zs)    = interpret xs ('.' : stack) input (z : output) (ys, z : zs)
interpret (',' : xs) stack [] output _                  = reverse $ (chr . fromIntegral) <$> output
interpret (',' : xs) stack (m : ms) output (ys, _ : zs) = interpret xs (',' : stack) ms output (ys, c : zs)
                                                          where c = (fromIntegral . ord) m
interpret ('[' : xs) stack input output (ys, z : zs)    = if z == 0
                                                          then interpret fxs newStack input output (ys, z : zs)
                                                          else interpret xs ('[' : stack) input output (ys, z : zs)
                                                          where (fxs, newStack)        = forward xs ('[' : stack) 0
                                                                forward (']' : xs) s 0 = (xs, ']' : s) 
                                                                forward (']' : xs) s d = forward xs (']' : s) (d - 1)
                                                                forward ('[' : xs) s d = forward xs ('[' : s) (d + 1)
                                                                forward (x : xs) s d   = forward xs (x : s) d
interpret (']' : xs) stack input output (ys, z : zs)    = if z /= 0
                                                          then interpret bxs newStack input output (ys, z : zs)
                                                          else interpret xs (']' : stack) input output (ys, z : zs)
                                                          where (bxs, newStack)     = back (']' : xs) stack 0
                                                                back xs ('[' : s) 0 = (xs, '[' : s)
                                                                back xs ('[' : s) d = back ('[' : xs) s (d - 1)
                                                                back xs (']' : s) d = back (']' : xs) s (d + 1)
                                                                back xs (x : s) d   = back (x : xs) s d

brainfuck :: String -> String -> String
brainfuck code input = interpret code [] input [] ([], [0])

