module Exercise4 where

import Data.Tree
import Employee
import Exercise1
import Exercise2
import Exercise3

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold nextLevel t
