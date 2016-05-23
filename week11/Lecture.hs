module Lecture(mapA, replicateA) where

import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
(*>) fa fb = fb

map' :: (a -> b) -> [a] -> [b]
map' f = foldr cons []
  where cons x ys = f x : ys

-- This is pretty similar to what we have for the pure map.
mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = foldr lift_cons (pure [])
  where lift_cons x ys = (:) <$> (f x) <*> ys 

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = mapA id

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = sequenceA $ replicate n fa
