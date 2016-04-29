-- insert :: a -> Tree a -> Tree a

-- balance :: Tree a -> Tree a

balanceFactor :: Tree a -> Tree a -> Height
balanceFactor (Node h1 _ _ _) (Node h2 _ _ _) = h1 - h2


-- Rotation operations: Rotate along the given node.
rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight pivot@(Node pivotHeight
                   left@(Node lHeight lLeft lValue lRight)
                   value
                   right@(Node rHeight rLeft rValue rRight)) =
  Node pivotHeight lLeft lValue (Node lHeight lRight value right)

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft pivot@(Node pivotHeight
                  left@(Node lHeight lLeft lValue lRight)
                  value
                  right@(Node rHeight rLeft rValue rRight)) =
  Node pivotHeight (Node rHeight left value rLeft) rValue rRight

-- Testing data
a = (Node 0 Leaf 'A' Leaf)
b = (Node 0 Leaf 'B' Leaf)
c = (Node 0 Leaf 'C' Leaf)
p = (Node 1 a 'P' b)
q = (Node 2 p 'Q' c)
