module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Show, Eq)


-- Ex 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2 


tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? n | n < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? n = xs !!? (n-1)


jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Sized means that we can pull a `Size` element out of the given element,
-- and Monoid means the same element can be appended with another
-- such element:
-- > Size 1 `mappend` Size 1
-- Size  2

-- Ex 2

sizeJL :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJL = getSize . size . tag


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty              = Nothing
indexJ n _  | n < 0         = Nothing
indexJ n jl | n > sizeJL jl = Nothing  -- Otherwise it returns the right element
indexJ _ (Single m a)       = Just a
indexJ n (Append s l r)
  | n < leftSize  = indexJ n l
  | otherwise     = indexJ (n - leftSize) r
  where leftSize  = sizeJL l


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a ->  JoinList b a
dropJ _ Empty            = Empty
dropJ _ (Single _ _)     = Empty
dropJ n jl
  | n == 0                   = jl
  | n < 0 || n >= sizeJL jl  = Empty
-- Pretty strange that +++ works below but not actual Append
dropJ n jl@(Append s l r)
  | n < leftSize   = (dropJ n l) +++ r
  | n == leftSize  = Empty +++ r
  | n > leftSize   = Empty +++ (dropJ (n - leftSize) r)
  where leftSize = sizeJL l
          

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a ->  JoinList b a
takeJ 0 _               = Empty
takeJ n jl
  | n >= sizeJL jl      = jl
takeJ _ jl@(Single _ _) = jl
takeJ n (Append s l r)
  | n == leftSize = l
  | n <  leftSize = takeJ n l
  | otherwise = l +++ takeJ (n - leftSize) r
  where leftSize = sizeJL l
        

e = Single (Size 1) 'e'
a = Single (Size 1) 'a'
o = Single (Size 1) 'o'
 

ea  = e +++ a
oea = o +++ e +++ a 


testIndex i jl = indexJ i jl == jlToList jl !!? i
testDrop  n jl = jlToList (dropJ n jl) == drop n (jlToList jl)
testTake  n jl = jlToList (takeJ n jl) == take n (jlToList jl)
