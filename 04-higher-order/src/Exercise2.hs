module Exercise2 where

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertIntoTree Leaf

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree x Leaf = Node 0 Leaf x Leaf
insertIntoTree x (Node h left v right)
    | (treeHeight left) < (treeHeight right) = let newLeft = insertIntoTree x left in Node ((treeHeight newLeft)+1) newLeft v right
    | otherwise = let newRight = insertIntoTree x right in Node ((treeHeight newRight)+1) left v newRight

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h