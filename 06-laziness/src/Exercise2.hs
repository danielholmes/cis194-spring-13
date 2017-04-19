module Exercise2 where

fibs2 :: [Integer]
fibs2 = 0 : map snd (iterate (\(a, b) -> (b, a + b)) (0, 1))
