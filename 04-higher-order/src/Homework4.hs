module Homework4 where

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram 0 = []
sieveSundaram n = map (+1) . map (*2) $ [1..n] \\ sieve
                    -- NOTE: sieve is too large, won't impact correctness but maybe performance
                    where sieve = [i + j + 2 * i * j | j <- [1..n], i <- [1..j]]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]