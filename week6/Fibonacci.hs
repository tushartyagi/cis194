module Fibonacci where

-- Ex 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- Ex 2

-- Each operation takes O(n) time
fib2 :: Integer -> Integer
fib2 n = fib2' 0 1 n
  where fib2' acc next n = if n == 0 then next
                           else fib2' next (acc + next) (n - 1)

fibs2 :: [Integer]
fibs2 = 0 : [fib2 n | n <- [0..]] 

-- Ex 3
-- data List' a = Empty
--              | Cons a (List' a)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show ss = show . take 20 $ streamToList ss

streamToList :: Stream a -> [a]
streamToList (Cons s ss) = s : streamToList ss

-- Ex 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons s ss) = Cons (f s) (streamMap f ss)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed rule seed = Cons seed (streamMap rule (streamRepeat seed))

-- Ex 5
-- nats :: Stream Integer
-- nats = streamFromSeed (+1) 0
