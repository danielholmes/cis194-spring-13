{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Exercise5 where

import Exercise3
import StackVM
import Parser

instance Expr Program where
    lit i = [PushI i]
    add a b = a ++ b ++ [Add]
    mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul