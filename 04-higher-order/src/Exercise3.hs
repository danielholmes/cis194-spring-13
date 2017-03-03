module Exercise3 where

xor :: [Bool] -> Bool
xor = foldr (/=) False
--xor = foldr (\x b -> if x then not b else b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\a b -> f b a) base xs