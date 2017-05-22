module Exercise3 where

import Employee
import Exercise1

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss divs = (yesBoss, noBoss)
    where
        yesBoss = glCons boss (mconcat (map snd divs))
        noBoss = mconcat $ map (uncurry moreFun) divs
