module Golf where

import Data.List.Split
import Data.List
import Data.Char

{-
  We cut the problem up into one iteration for each valid skip amount. i.e. 1 up to the length of the list since
  anything higher than that results in an empty list.
  Each iteration uses the chop function which acts as a kind of map + drop function. i.e. we take an element then
  drop the next n elements
-}
skips :: [a] -> [[a]]
skips l = [chop (\(x:xs) -> (x, drop (n-1) xs)) (drop (n-1) l) | n <- [1..(length l)]]

{-
  We first convert the list to sets of 3 using divvy. The middle element in each set is a possible candidate for being
  a local maxima. We filter to find these locals then map to just include them
-}
localMaxima :: [Integer] -> [Integer]
localMaxima x = map (!! 1) (filter (\[a,b,c] -> b > a && b > c) (divvy 3 1 x))
--localMaxima x = map (!! 1) (filter (\[a,b,c] -> b > a && b > c) (divvy 3 1 x))
--localMaxima x = foldr (\[a,b,c] x -> if b > a && b > c then b:x else x) [] (divvy 3 1 x)
--localMaxima x = map (\[_,b,_] -> b) (filter (\[a,b,c] -> b > a && b > c) (divvy 3 1 x))

{-
  1. We get the number of occurrences of each possible num, paired with the num itself in c
  2. We find the maximum (m) which tells us how high each column needs to be
  3. with each count we create each column, then transpose the resulting table to allow for correct string output
-}
histogram :: [Integer] -> String
histogram l = unlines $ transpose [replicate (m-d) ' ' ++ replicate d '*' ++ "=" ++ show n | (n,d) <- c]
                where
                    c = [(n, length $ (filter (==n)) l) | n <- [0..9]]
                    m = maximum (map snd c)

{-
  1. Find the counts of each possible number (c)
  2. Create each row initialised with it's number (reverse [1..(maximum c)])
  3. Map each row number to a row of characters, one for each possible number. Use * if the row should show a dot (the
     count for this column number is >= the row number) otherwise ' '
  4. Join this together with the base x axis0
-}
--histogram l = (unlines r) ++ "==========\n0123456789\n"
--                where
--                    c = [(length . (filter (==n))) l | n <- [0..9]]
--                    r = map (\r -> [if d >= r then '*' else ' ' | d <- c]) (reverse [1..(maximum c)])