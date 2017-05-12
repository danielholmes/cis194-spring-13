{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Exercise3 where

import Data.Monoid
import Data.Char
import JoinList

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

score :: Char -> Score
score c
    | isLower c = score . toUpper $ c
    | elem c "EAIONRTLSU" = Score 1
    | elem c "DG" = Score 2
    | elem c "BCMP" = Score 3
    | elem c "FHVWY" = Score 4
    | elem c "K" = Score 5
    | elem c "JX" = Score 8
    | elem c "QZ" = Score 10
    | otherwise = mempty

scoreString :: String -> Score
scoreString = foldr (\a b -> (score a) <> b) mempty

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine x = Single (scoreString x) x
