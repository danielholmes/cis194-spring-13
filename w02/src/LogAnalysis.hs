{-# OPTIONS_GHC -Wall #-}

module LogAnalysis (
    parse,
    parseMessage,
    parseMessageWithType,
    insertLog,
    buildLog,
    inOrder,
    whatWentWrong
) where

import Log
import Data.Char
import Data.List

parseMessage :: String -> LogMessage
parseMessage m = case result of
    Nothing -> Unknown m
    Just (mt, ts, xs) -> LogMessage mt ts (intercalate " " xs)
    where
        comps = words m
        result = parseMessageWithTimestamp comps

isInteger :: String -> Bool
isInteger n = all isDigit n

parseMessageWithTimestamp :: [String] -> Maybe (MessageType, TimeStamp, [String])
parseMessageWithTimestamp m = case (parseMessageWithType m) of
    Nothing -> Nothing
    Just (_, []) -> Nothing
    Just (mt, (x:xs)) -> if isInteger x then Just (mt, (read x), xs) else Nothing

parseMessageWithType :: [String] -> Maybe (MessageType, [String])
parseMessageWithType ("E":c:xs) = if isInteger c then Just ((Error (read c)), xs) else Nothing
parseMessageWithType ("W":xs) = Just (Warning, xs)
parseMessageWithType ("I":xs) = Just (Info, xs)
parseMessageWithType _ = Nothing

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

insertLog :: LogMessage -> MessageTree -> MessageTree
insertLog m t
    | (Unknown _) <- m = t
    | (LogMessage _ ts _) <- m =
        case t of
            Leaf -> Node Leaf m Leaf
            (Node treeLeft tm@(LogMessage _ treeTs _) treeRight) ->
                if ts < treeTs then (Node (insertLog m treeLeft) tm treeRight)
                else (Node treeLeft tm (insertLog m treeRight))
            (Node _ (Unknown _) _) -> error "Shouldn't be a node with unknown"

buildLog :: [LogMessage] -> MessageTree
buildLog [] = Leaf
buildLog (x:xs) = insertLog x (buildLog xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ m : (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ms = severeErrors (inOrder (buildLog ms))

severeErrors :: [LogMessage] -> [String]
severeErrors ms
    | ms == [] = []
    | ((LogMessage (Error c) _ em):xs) <- ms, c >= 50 = em : severeErrors xs
    | otherwise = severeErrors (tail ms)