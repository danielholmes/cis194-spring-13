module Exercise2 where

import Data.Tree

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold op initial (Node label []) = op label initial
treeFold op initial (Node label (t:ts)) = treeFold op (treeFold op initial (Node label ts)) t
