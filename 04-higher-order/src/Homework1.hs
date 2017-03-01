module Homework1 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
--fun1' = foldr (\n x -> if even n then (n-2) * x else x) 1
fun1' = foldr (*) 1 . map (\n -> if even n then n-2 else 1)

fun2' :: Integer -> Integer
fun2' n = sum ((filter even . takeWhile (/=1) . iterate fun2Input) n)

fun2Input :: Integer -> Integer
fun2Input n
    | even n = n `div` 2
    | otherwise = 3 * n + 1