module Lecture where

typeChanger :: (b -> c) -> (a -> b) -> (a -> c)
typeChanger bToC aToB = bToC . aToB
