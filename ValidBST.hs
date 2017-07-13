-- Valid Binary Search Tree @ HackerRank
-- Manoel Vilela
-- Date: Thu 13 Jul 2017 02:29:41 PM -03

module Main where

import Control.Monad (replicateM)

data Tree = Leaf | Node Int Tree Tree

prettyTree :: Tree -> Int -> String
prettyTree Leaf _ = ""
prettyTree (Node a left right) d = (replicate d ' ') ++ "=> " ++ show a ++ "\n"
                                   ++ lookBranch left ++ lookBranch right
  where lookBranch b = prettyTree b (d + 3)

instance Show Tree where
  show t = prettyTree t 0


insert :: Int  -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n tree@(Node a l r)
  | n == a = tree
  | n > a = Node a l (insert n r)
  | n < a = Node a (insert n l) r


preorder :: Tree -> [Int]
preorder Leaf = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r


construct :: [Int] -> Tree
construct [] = Leaf
construct (x:xs) = insert x $ construct xs

isBST :: [Int] -> Bool
isBST xs = xs == (preorder $ construct $ reverse xs)

message :: Bool -> String
message True = "YES"
message False = "NO"

getList :: IO [Int]
getList = do _ <- getLine
             input <- getLine
             let nums = map (read :: String -> Int) $ words input
             return (nums)

main :: IO ()
main = do n <- getLine
          lists <- replicateM (read n :: Int) getList
          mapM_ (putStrLn) $ map (message . isBST) lists
