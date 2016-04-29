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

foldTree :: (Eq a) => [a] -> Tree a
foldTree = foldr insert Leaf

insert :: (Eq a) => a -> Tree a -> Tree a
insert nodeValue Leaf = Node 0 Leaf nodeValue Leaf
insert nodeValue tree@(Node treeHeight left value right) 
  | left == Leaf = Node (treeHeight + 1) (insert nodeValue left) value right
  | right == Leaf = Node treeHeight left value (insert nodeValue right)
  | height left < height right = Node treeHeight (insert nodeValue left) value right
  | height left > height right = Node treeHeight left value (insert nodeValue right)
  | countNodes left < countNodes right = Node treeHeight (insert nodeValue left) value right
  | countNodes left > countNodes right = Node treeHeight left value (insert nodeValue right)
  | otherwise = let newNode = insert nodeValue left
                    newHeight = 1 + (height newNode `max` height right)
                in Node newHeight newNode value right


height :: Tree a -> Integer
height Leaf = (-1) 
height (Node h _ _ _) = h

countNodes :: Tree a -> Int
countNodes Leaf = 0 
countNodes (Node _ left _ right) = countNodes left + 1 + countNodes right


instance (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show node = "\n" ++ showWithIndent node 0

showWithIndent Leaf n = tabs n ++ "Leaf"
showWithIndent (Node height left value right) n = 
            tabs n ++ "(Node " ++ show height ++ "\n" ++
            tabs n ++ showWithIndent left  (n+1) ++ "\n" ++
            tabs (n+4) ++ show value ++ "\n" ++
            tabs n ++ showWithIndent right (n+1) ++ ")"

tabs n = concat (replicate n "  ")


hasNoChild Leaf = True
hasNoChild (Node _ Leaf _ Leaf) = True
hasNoChild _ = False

-- Exercise 3

xor :: [Bool] -> Bool
xor = odd . foldr countTrue 0
  where countTrue = \current acc -> if current then acc + 1 else acc

map' :: (a -> b) -> [a] -> [b]
map' f = foldr add []
  where add current acc = [f current] ++ acc

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr f' base (reverse xs)
  where f' current acc = f acc current

-- Exercise 4

list1 n = [i + j + 2*i*j | i <- [1..n], j <- [1..n], i <= j, i + j + 2*i*j <= n]
list2 n = filter (<= n) . map (\(i, j) -> i + j + 2*i*j) $
  [(i,j) | i <- [1..n], j <- [1..n], i <= j]


elems n = [x | x <- [1..n], not (x `elem` ijValues)]
  where ijValues = list1 n

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) (elems n)
