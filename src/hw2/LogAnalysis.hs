-- HW2, ADTs https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Prelude

parseMessage :: String -> LogMessage
parseMessage s = case words s of
                    "E":sev:t:m -> LogMessage (Error (read sev :: Int)) (read t :: Int) (unwords m)
                    "W":t:m -> LogMessage Warning (read t :: Int) (unwords m)
                    "I":t:m -> LogMessage Info (read t :: Int) (unwords m)
                    _ -> Unknown s

parse :: String -> [LogMessage]
parse contents = fmap parseMessage (lines contents)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ t _) tree = case tree of
        Leaf -> Node Leaf m Leaf
        Node Leaf m'@(LogMessage _ t' _) Leaf -> if t > t' then Node Leaf m' (insert m Leaf) else Node (insert m Leaf) m' Leaf
        Node Leaf m'@(LogMessage _ t' _) tree'@(Node _ _ _) -> if t > t' then Node Leaf m' (insert m tree') else Node (Node Leaf m Leaf) m' tree'
        Node tree'@(Node _ _ _) m'@(LogMessage _ t' _) Leaf -> if t > t' then Node tree' m' (Node Leaf m Leaf) else insert m tree'
        Node tree'@(Node _ _ _) m'@(LogMessage _ t' _) tree''@(Node _ _ _) -> if t > t' then Node tree' m' (insert m tree'') else Node (insert m tree') m' tree''

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder n = case n of
        Node Leaf m Leaf -> [m]
        Node Leaf m tree'@(Node _ _ _) -> [m] ++ inOrder tree'
        Node tree'@(Node _ _ _) m Leaf ->  inOrder tree' ++ [m]
        Node tree'@(Node _ _ _) m tree''@(Node _ _ _) -> inOrder tree' ++ [m] ++ inOrder tree''