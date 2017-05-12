{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Exercise4 where

--import JoinList
--import Exercise3
--import Exercise2
--
--instance Buffer JoinList (Score, Size) String
--  toString   x = ""
--  fromString s = foldr (\a b -> b +++ scoreLine a) Empty (lines s)
--  line         = indexJ
--  replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
--      where replaceLine' pre [] = pre
--            replaceLine' pre (_:ls) = pre ++ l:ls
--  numLines     = getSize . snd . tag
--  value        = fst . tag
