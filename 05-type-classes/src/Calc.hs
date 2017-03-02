module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    add = Add
    mul = Mul
    lit = Lit

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