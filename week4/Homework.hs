module Homework where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even 


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- fun2' :: Integer -> Integer

-- Exercise 2
type Height = Integer

data Tree a = Leaf
            | Node Height (Tree a) a (Tree a)
            deriving Eq

-- foldTree :: [a] -> Tree a
-- foldTree = foldr createTree Leaf

-- Node will have a height of 0
insert :: (Eq a) => a -> Tree a -> Tree a
insert nodeValue Leaf = Node 0 Leaf nodeValue Leaf
insert nodeValue tree@(Node treeHeight left value right) 
  | left == Leaf = Node (treeHeight + 1) (insert nodeValue left) value right
  | right == Leaf = Node treeHeight left value (insert nodeValue right)
  | height left < height right = Node treeHeight (insert nodeValue left) value right
  | height left > height right = Node treeHeight left value (insert nodeValue right)
  | otherwise = Node treeHeight (insert nodeValue left) value right

instance (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show node = "\n" ++ showWithIndent node 0

showWithIndent Leaf n = tabs n ++ "Leaf"
showWithIndent (Node height left value right) n = 
            tabs n ++ "(Node " ++ show height ++ "\n" ++
            tabs n ++ showWithIndent left  (n+1) ++ "\n" ++
            tabs (n+2) ++ show value ++ "\n" ++
            tabs n ++ showWithIndent right (n+1) ++ ")"

tabs n = concat (replicate n "  ")

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

