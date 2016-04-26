{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1 starts

parseMessage :: String -> LogMessage
parseMessage m = case words m of
  ("E":s:t:msg) -> LogMessage (Error (read s)) (read t) (unwords msg)
  ("I":t:msg) -> LogMessage Info (read t) (unwords msg)
  ("W":t:msg) -> LogMessage Warning (read t) (unwords msg)
  msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines


-- Exercise 2 starts

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTime _) (Node left treeMsg@(LogMessage _ treeTime _) right)
  | msgTime <= treeTime = Node (insert msg left) treeMsg right
  | msgTime > treeTime = Node left treeMsg (insert msg right)

-- Exercise 3 starts

build :: [LogMessage] -> MessageTree
build msgs = foldr buildTree Leaf msgs
  where buildTree msg tree = insert msg tree

-- Exercise 4 starts

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5 starts

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = [msg | (LogMessage (Error severity) _ msg) <- inOrder (build msgs), severity >= 50]

-- Exercise 6 starts
{- Not really sure if this is a reference to pop culture, or I am simply
thinking along wrong lines, but searching for >= 50 severity msg in error.log
gives me:

["Mustardwatch opened, please close for proper functioning!","All backup mustardwatches are busy","Depletion of mustard stores detected!","Hard drive failure: insufficient mustard","All backup mustardwatches are busy","Twenty seconds remaining until out-of-mustard condition","Ten seconds remaining until out-of-mustard condition","Empty mustard reservoir! Attempting to recover...","Recovery failed! Initiating shutdown sequence"]

Some mustard guy??

-}
