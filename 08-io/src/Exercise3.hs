module Exercise3 where

import Employee
import Exercise1

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss divs = (yesBoss, noBoss)
    where
        divsYesSubBosses = mconcat (map fst divs)
        divsNoSubBosses = mconcat (map snd divs)
        yesBoss = glCons boss divsNoSubBosses
        noBoss = moreFun divsNoSubBosses divsYesSubBosses
