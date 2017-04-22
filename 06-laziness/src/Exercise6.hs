{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Exercise6 where

import Exercise3
import Exercise4
import Exercise5

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)
    negate (Stream n ns) = Stream (negate n) (negate ns)
    (+) (Stream x xs) (Stream y ys) = Stream (x + y) (xs + ys)
    (*) (Stream x xs) (Stream y ys) = Stream (x * y) ((streamMap (*x) ys) + (streamMap (*y) xs))

instance Fractional (Stream Integer) where
    (/) (Stream x xs) (Stream y ys) = q
        where q = Stream (x `div` y) (streamMap (`div` y) (xs - ys * q))

fibs3 :: Stream Integer
fibs3 = nats / (1 - nats - (nats * nats))
