-- Problem: Calculator
-- Rank: 3
-- Src: https://www.codewars.com/kata/5235c913397cbf2508000048
-- Note: simple usage of parsec

module Calculator where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

expr :: Parser Double
expr = buildExpressionParser table factor

table = [[op "*" (*) AssocLeft, op "/" (/) AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
        where op s f assoc = Infix (do {string s; return f}) assoc

factor = do { char '('
            ; x <- expr
            ; char ')'
            ; return x
            }
        <|> number

number :: Parser Double
number = do dx <- many1 $ oneOf "0123456789."
            return $ read dx

evaluate :: String -> Double
evaluate s = case parse expr "" (concat $ words s) of
             Right x -> x
             Left _ -> undefined
