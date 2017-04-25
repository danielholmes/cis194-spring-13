{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Exercise7 where

data Matrix = Matrix Integer Integer Integer Integer deriving (Eq, Show)

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix e f g h) = Matrix ((a * e) + (b * g)) ((a * f) + (b * h)) ((c * e) + (d * g)) ((c * f) + (d * h))

fib4 :: Integer -> Integer
fib4 n = d where (Matrix _ _ _ d) = (Matrix 1 1 1 0) ^ (n + 1)
