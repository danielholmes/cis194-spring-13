{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exercise6 where

import qualified Data.Map as M
import Exercise3

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
    | Add VarExprT VarExprT
    | Mul VarExprT VarExprT
    | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    add = Add
    mul = Mul
    lit = Lit

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = const . Just
    {-add op1 op2 = \s -> case (op1 s, op2 s) of
                            (Just a, Just b) -> Just (a + b)
                            _ -> Nothing-}
    {-mul op1 op2 = \s -> case (op1 s, op2 s) of
                                  (Just a, Just b) -> Just (a * b)
                                  _ -> Nothing-}
    -- Must be a nicer way to do this
    add op1 op2 = \s -> let a = op1 s; b = op2 s in a >>= (\x -> fmap (+x) b)
    mul op1 op2 = \s -> let a = op1 s; b = op2 s in a >>= (\x -> fmap (*x) b)

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs expr = expr $ M.fromList vs