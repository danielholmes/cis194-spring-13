module Exercise5 where

import Exercise3
import Exercise4

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = (Stream x (interleaveStreams ys xs))

streamIterate :: Stream (Stream Integer) -> Stream Integer
streamIterate (Stream x xs) = interleaveStreams x (streamIterate xs)

rulerStep :: Integer -> Stream Integer
rulerStep n = interleaveStreams (streamRepeat n) (rulerStep (n+1))

ruler :: Stream Integer
ruler = rulerStep 0
