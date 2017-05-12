module Exercise2 where

import JoinList
import Sized
import Exercise1

getMonoidSize :: (Monoid m, Sized m) => JoinList m a -> Int
getMonoidSize Empty = getSize mempty
getMonoidSize (Single m _) = getSize (size m)
getMonoidSize (Append m _ _) = getSize (size m)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ j (Append _ left right)
    | j < getMonoidSize left = indexJ j left
    | otherwise = indexJ (j - (getMonoidSize left)) right

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 x = x
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append _ left right)
    | i > getMonoidSize left = dropJ (i - (getMonoidSize left)) right
    | otherwise = (dropJ i left) +++ right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ a@(Single _ _) = a
takeJ i (Append _ left right)
    | i < getMonoidSize left = takeJ i left
    | otherwise = left +++ (takeJ (i - (getMonoidSize left)) right)
