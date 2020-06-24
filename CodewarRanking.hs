-- Problem: Codewars style ranking system
-- Rank: 4
-- Src: https://www.codewars.com/kata/51fda2d95d6efda45e00004e

module CodewarRanking where

data User = User {getRank :: Int, getProgress :: Int}

gap :: Int -> Int -> Int
gap r1 r2 = aux r1 - aux r2
            where aux x = if x < 0 then x + 8 else x + 7

toProgress :: Int -> Int
toProgress 0    = 3
toProgress (-1) = 1
toProgress g    = if g < 0 then 0 else 10 * g * g

rankUp :: User -> User
rankUp (User 8 p)           = User 8 0
rankUp (User r p) | p < 100 = User r p
rankUp (User (-1) p)        = rankUp $ User 1 (p - 100)
rankUp (User r p)           = rankUp $ User (r + 1) (p - 100)

newUser :: User
newUser = User (-8) 0

rank :: User -> Int
rank = getRank

progress :: User -> Int
progress = getProgress

incProgress :: Int -> User -> User
incProgress r1 (User r2 p) | r1 < (-8) || r1 == 0 || r1 > 8 = error "Out of rank." 
                           | otherwise                      = rankUp $ User r2 newP
                             where newP = p + (toProgress $ gap r1 r2)

