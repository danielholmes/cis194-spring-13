module Exercise2 where

import Exercise1
import ExprT
import Parser

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul