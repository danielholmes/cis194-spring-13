{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exercise4 where

import Exercise3

instance Expr Integer where
    add = (+)
    mul = (*)
    lit = id

instance Expr Bool where
    add = (||)
    mul = (&&)
    lit = (>0)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)
    lit = MinMax

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
    lit a = Mod7 (a `mod` 7)