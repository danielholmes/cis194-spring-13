module Exercise2 where

import Data.Tree

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold op (Node label ts) = op label $ map (treeFold op) ts
