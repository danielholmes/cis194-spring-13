module Homework4 where

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram 0 = []
sieveSundaram n = map (+1) . map (*2) . filter (\x -> notElem x sieve) $ [1..n]
                    where sieve = map (\(i,j) -> i + j + 2 * i * j) . filter (\(i,j) -> i <= j) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]