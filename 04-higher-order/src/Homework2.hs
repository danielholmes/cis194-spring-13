module Homework2 where

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree x = go x Leaf

go :: [a] -> Tree a -> Tree a
go (x:xs) accu = go xs (insertIntoTree x accu)
go [] accu = accu

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree x Leaf = Node 0 Leaf x Leaf
insertIntoTree x (Node h left v right)
    | (treeHeight left) < (treeHeight right) = (Node (h+1) (insertIntoTree x left) v right)
    | otherwise = (Node (h+1) left v (insertIntoTree x right))

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h