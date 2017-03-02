module Homework3 where

xor :: [Bool] -> Bool
xor = foldr (\x b -> if x then not b else b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []