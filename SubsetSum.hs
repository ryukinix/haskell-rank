-- Subset Sum @ HackerRank
-- Date: Fri 14 Jul 2017 07:31:23 PM -03
-- Manoel Vilela

import Data.List (sortBy)
import Control.Monad (replicateM)

-- Binary Tree of type n
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq)

-- Just a show method to debug any Tree
prettyTree :: (Show a) => Tree a -> Int -> String
prettyTree Leaf _ = ""
prettyTree (Node a left right) d = (replicate d ' ') ++ "=> " ++ show a ++ "\n"
                                   ++ lookBranch left ++ lookBranch right
  where lookBranch b = prettyTree b (d + 3)

instance (Show a) => Show (Tree a) where
  show t = prettyTree t 0

-- Binary Search Insert
insert :: (Ord a, Eq a) => a  -> Tree a -> Tree a
insert n Leaf = Node n Leaf Leaf
insert n tree@(Node a l r)
  | n == a = tree
  | n > a = Node a l (insert n r)
  | n < a = Node a (insert n l) r

-- Binary Search Insert Balanced
insertBalanced :: (Eq a, Ord a) => [a] -> Tree a -> Tree a
insertBalanced [] t = t
insertBalanced [x] t = insert x t
insertBalanced xs t = let pivot = xs !! div (length xs) 2
                          newNode = insert pivot t
                          l = [k | k <- xs, k < pivot]
                          r = [k | k <- xs, k > pivot]
                          in insertBalanced l (insertBalanced r newNode)

-- TODO: O(log(n)) [DONE]
searchTreeNode :: Tree ((Int,Int),Int) -> Int -> Int
searchTreeNode Leaf _ = -1
searchTreeNode (Node ((a,b),v) l r) n
  | a < n && n <= b = v
  | n < b = searchTreeNode l n
  | n > b = searchTreeNode r n

-- O(n)
-- generate the range of SubSets
-- ((lower_bound, upper_bound), subset_size)
subSetSumList :: [Int] -> [((Int, Int), Int)]
subSetSumList l = func l 0 1
  where func [] _ _ = []
        func (x:xs) s d = let v = x + s
                          in ((s,v),d):func xs v (d+1)


-- TODO: annotate the max sum to skip the bad cases [DONE]
-- TODO: do binary search by preparing the data that way:
--       pre-compute all the pairs (min_sum,max_sum) of max-sum depth (max-sum)
--       create a binary tree to do a search [DONE]
main :: IO()
main = do _ <- getLine -- ignore, size of the set in the next line
          input <- getLine -- the initial set
          n <- readLn -- n queries
          tests <- replicateM n readLn -- read them
          let xs = sortBy (flip compare) $ map read $ words input :: [Int]
          let bst = insertBalanced (subSetSumList xs) Leaf
          mapM_ print $ map (searchTreeNode bst) tests
