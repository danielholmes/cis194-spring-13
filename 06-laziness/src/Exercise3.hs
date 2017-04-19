module Exercise3 where

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show x = show (take 20 (streamToList x))

streamToList :: Stream a -> [a]
streamToList (Stream a b) = a : streamToList b
