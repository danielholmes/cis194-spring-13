module Exercise4 where

import Exercise3

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap op (Stream x xs) = Stream (op x) (streamMap op xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed op a = Stream a (streamFromSeed op (op a))
