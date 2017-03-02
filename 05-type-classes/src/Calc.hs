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

{- Bool Expressions
data ExprBool = LitB Integer
    | AddB ExprBool ExprBool
    | MulB ExprBool ExprBool
    deriving (Show, Eq)

instance Expr Bool where
    add = AddB
    mul = MulB
    lit = LitB

evalBool :: ExprBool -> Bool
evalBool (LitB x) = x > 0
evalBool (AddB a b) = (evalBool a) || (evalBool b)
evalBool (MulB a b) = (evalBool a) && (evalBool b)-}