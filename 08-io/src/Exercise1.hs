{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exercise1 where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = ef }) (GL l f) = GL (e : l) (f + ef)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l1@(GL _ f1) l2@(GL _ f2)
    | f1 > f2 = l1
    | otherwise = l2
