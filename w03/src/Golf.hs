module Golf where

import Data.List.Split

skips :: [a] -> [[a]]
skips x = map (\i -> skipAmount i x) [0..((length x)-1)]

skipAmount :: Int -> [a] -> [a]

--skipAmount n list = case comps of
--    (_, []) -> []
--    (_, (x:xs)) -> x : skipAmount n xs
--    where comps = splitAt n list