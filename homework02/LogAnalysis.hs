{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Homework 02

-- Parses one line from our log file into the correct LogMessage.
parseMessage :: String -> LogMessage
parseMessage [] = Unknown []
parseMessage line = if isValid line then parseMessage' else Unknown line
    where parseMessage' = case messageType
             of Just m@(Error _) -> LogMessage m (timeStamp 2) (message 3)
                Just m           -> LogMessage m (timeStamp 1) (message 2)
                Nothing          -> Unknown line
          messageType   = case word 0 line
                          of "I" -> Just Info
                             "W" -> Just Warning
                             "E" -> Just (Error . read $ word 1 line)
                             _   -> Nothing
          timeStamp n   = read $ word n line
          message n     = unwords . drop n $ words line

-- Parses a log String consisting of lines from our log file
parse :: String -> [LogMessage]
parse [] = []
parse s  = map parseMessage $ lines s

-- Returns the nth word of a String. Words are separated by spaces.
word :: Int -> String -> String
word _ [] = []
word n s  = (!!n) $ words s

-- Checks whether the String from the log is correctly formatted.
isValid :: String -> Bool
isValid [] = False
isValid s = hasFields 2 && hasMetaInfo
    where hasFields n    = (>= n) . length $ words s
          hasMetaInfo    = case word 0 s
                           of "I" -> hasNumField 1
                              "W" -> hasNumField 1
                              "E" -> hasFields 3 && hasNumField 1 &&
                                     hasNumField 2
                              _   -> False
          hasNumField n = case reads $ word n s :: [(Int, String)]
                          of [(_,"")] -> True
                             _        -> False

-- Inserts a LogMessage into its correct spot in a MessageTree.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf         = Node Leaf msg Leaf
insert msg@(LogMessage _ tIns _) (Node ltree nMsg@(LogMessage _ tCom _) rtree)
    | tIns < tCom  = Node (insert msg ltree) nMsg rtree
    | tIns > tCom  = Node ltree nMsg (insert msg rtree)
    | tIns == tCom = Node ltree msg rtree
insert _ tree           = tree

-- Creates a BST from a list of LogMessages.
build :: [LogMessage] -> MessageTree
build logs = foldl (\tree msg -> insert msg tree) Leaf logs

-- Traverses a MessageTree in order and returns a list of LogMessages.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree msg rtree) = inOrder ltree ++ [msg] ++ inOrder rtree

-- Returns a sorted list of error messages with an importance of 50 or greater.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = map message . filter importantError . inOrder $ build xs
    where message (LogMessage _ _ msg)              = msg
          message (Unknown msg)                     = msg
          importantError (LogMessage (Error i) _ _) = i >= 50
          importantError _                          = False
