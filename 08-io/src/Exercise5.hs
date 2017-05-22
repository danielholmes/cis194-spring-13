module Exercise5 where

import Data.List
import Employee
import Exercise4

companyMain :: IO ()
companyMain = readFile "company.txt" >>= putStrLn . guestListToStr . maxFun . read

guestListToStr :: GuestList -> String
guestListToStr (GL e f) = unlines $ ["Total fun: " ++ (show f)] ++ (sort (map empName e))
