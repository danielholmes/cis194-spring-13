{-# OPTIONS_GHC -Wall #-}

module LogAnalysis (parse, parseMessage, parseMessageWithType) where

import Log
import Data.Char
import Data.List

parseMessage :: String -> LogMessage
parseMessage m = case result of Nothing -> Unknown m
                                Just (mt, ts, xs) -> LogMessage mt ts (intercalate " " xs)
    where
        comps = words m
        result = parseMessageWithTimestamp comps

isInteger :: String -> Bool
isInteger n = all isDigit n

parseMessageWithTimestamp :: [String] -> Maybe (MessageType, TimeStamp, [String])
parseMessageWithTimestamp m = case (parseMessageWithType m) of Nothing -> Nothing
                                                               Just (_, []) -> Nothing
                                                               Just (mt, (x:xs)) -> if isInteger x then Just (mt, (read x), xs) else Nothing

parseMessageWithType :: [String] -> Maybe (MessageType, [String])
parseMessageWithType ("E":c:xs) = if isInteger c then Just ((Error (read c)), xs) else Nothing
parseMessageWithType ("W":xs) = Just (Warning, xs)
parseMessageWithType ("I":xs) = Just (Info, xs)
parseMessageWithType _ = Nothing

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)