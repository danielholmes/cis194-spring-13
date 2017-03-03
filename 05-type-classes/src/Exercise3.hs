module Exercise3 where

import ExprT

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    add = Add
    mul = Mul
    lit = Lit