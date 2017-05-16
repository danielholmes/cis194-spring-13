{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exercise4 where

import Buffer
import Sized
import JoinList
import Exercise1
import Exercise2
import Exercise3
import Debug.Trace

joinListToString :: JoinList m String -> String
joinListToString Empty = ""
joinListToString (Single _ s) = s
joinListToString (Append _ left right) = (joinListToString left) ++ (joinListToString right)

getScore :: Score -> Int
getScore (Score i) = i

mapJoinListTag :: (Monoid m, Monoid n) => (m -> n) -> JoinList m String -> JoinList n String
mapJoinListTag _ Empty = Empty
mapJoinListTag op (Single m s) = Single (op m) s
mapJoinListTag op (Append m left right) = Append (op m) (mapJoinListTag op left) (mapJoinListTag op right)

scoreAndSizeLine :: String -> JoinList (Score, Size) String
scoreAndSizeLine s = mapJoinListTag (\m -> (m, Size 1)) (scoreLine s)

joinLists :: [JoinList (Score, Size) String] -> JoinList (Score, Size) String
joinLists [] = Empty
joinLists [x] = x
joinLists x = (fromLists left) +++ (fromLists right)
                  where
                      mid = length x `div` 2
                      (left, right) = splitAt mid x

instance Buffer (JoinList (Score, Size) String) where
    toString     = joinListToString
    fromString   = joinLists . map scoreAndSizeLine . lines
    line         = indexJ
    replaceLine n l b
        | n < numLines b = (takeJ n b) +++ (scoreAndSizeLine l) +++ (dropJ (n + 1) b)
        | otherwise = b
    numLines     = getSize . snd . tag
    value        = getScore . fst . tag
