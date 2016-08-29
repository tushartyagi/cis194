data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

treeSize Empty = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum Empty = 0
treeSum (Node l v r) = v + treeSum l + treeSum r

treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten Empty = []
flatten (Node l v r) = flatten l ++ [v] ++ flatten r

-- e : the default value
-- a : type of the default value, and the value returned by applying f
-- b : type of the elements in the Tree
treeFold :: a -> (a -> b -> a -> a) -> Tree b -> a 
treeFold e f Empty = e
treeFold e f (Node l v r) = f (treeFold e f l) v (treeFold e f r)

treeSize' = treeFold 0 (\l _ r -> 1 + l + r)
treeSum' = treeFold  0 (\l v r -> v + l + r)
treeDepth' = treeFold  0 (\l v r -> 1 + max l r) 


-- Monoids
-- 2 ways of creating newtypes
newtype Sum a = Sum a 
              deriving (Eq, Ord, Read, Show, Bounded)

getSum :: Sum a -> a
getSum (Sum a) = a

newtype Product a = Product { getProduct :: a } 
                  deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0
  Sum a `mappend` Sum b = Sum (a + b)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1 
  Product a `mappend` Product b = Product (a * b)


newtype Any = Any { getAny :: Bool } 
            deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool }
            deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
  mempty = All True 
  All x `mappend` All y = All (x && y)

