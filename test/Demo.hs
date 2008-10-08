module Demo where

import qualified Data.Number.MPFR.Up as M

-- compute the sum from 1 to n with precision of p bits rounded to Near
s     :: M.Precision -> Int -> M.MPFR
s p n = s' 1 0 
    where s' k acc | k <= n = s' (succ k) (M.add M.Near p acc (M.fromInt M.Near 32 k))
                   | otherwise = acc

s'    :: M.Precision -> Int -> M.MPFR
s' p  = foldl (M.addi M.Near p) 0 . enumFromTo 1

s'' :: M.Precision -> Int -> M.MPFR
s'' p = foldl ((. M.fromInt M.Up p) . (+)) M.zero . enumFromTo 1

-- compute pi with precision of n bits
pi' n = M.pi M.Near n

-- compute pi and get an indicator where the result is rounded
pi'' n = fst (M.mpfrToString M.Near 0 10 p) ++ case compare i 0 of 
                                                 GT -> " result is rounded up"
                                                 EQ -> " result is exact" 
                                                 LT -> " result is rounded down"
    where (p, i) = M.pi_ M.Near n



-- sum up first n terms of a Taylor series for e with precision p
e     :: M.Precision -> Int -> M.MPFR
e p n = e' 1 1 1
    where e' k acc acc' | k == n = acc
                        | otherwise= e' (succ k) (M.add M.Down p acc (M.div M.Down p M.one acc'')) acc''
                        where acc'' = M.muli M.Up p acc' k

-- factorial of n with p bits rounded to Near
fac     :: M.Precision -> Int -> M.MPFR
fac p n = s' 1 1 
    where s' k acc | k <= n = s' (succ k) (M.muli M.Near p acc k)
                   | otherwise = acc

