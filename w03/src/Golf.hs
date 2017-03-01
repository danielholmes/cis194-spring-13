module Golf where

import Data.List.Split
import Data.List
import Data.Char

skips :: [a] -> [[a]]
skips x = map (skipAmount x) [1..(length x)]

skipAmount :: [a] -> Int -> [a]
skipAmount l n = chop (\(x:xs) -> (x, drop (n-1) xs)) (drop (n-1) l)

-- skipAmount n x = map last (filter (\l -> length l == n) (chunksOf n x))

--skipAmount n list = case comps of
--    (_, []) -> []
--    (_, (x:xs)) -> x : skipAmount n xs
--    where comps = splitAt n list

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_:b:_) -> b) (filter (\(a:b:c:_) -> b > a && b > c) (divvy 3 1 xs))

-- localMaxima (a:as@(b:c:_)) = if b > a && b > c
--                             then b : localMaxima as
--                             else localMaxima as
--localMaxima _ = []

histogram :: [Integer] -> String
histogram l = intercalate "\n" (rows ++ base)
                where
                    nums = [0..9]
                    base = map (\_ -> '=') nums : map (intToDigit . fromIntegral) nums : []
                    counts = map (\n -> (length . (filter (==n))) l) nums
                    rows = map (\r -> map (\c -> if c >= r then '*' else ' ') counts) (reverse [1..(maximum counts)])