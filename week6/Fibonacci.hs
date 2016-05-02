{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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
streamFromSeed rule seed = Cons seed (streamFromSeed rule (rule seed)) 

-- Ex 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Counting Numbers: Natural Numbers - {0}
counting :: Stream Integer
counting = streamFromSeed (+1) 1

evenlyDivision :: Integer -> Integer
evenlyDivision n = last . powers $ 2*n
  where powers n = powersOf2 n 1 

powersOf2 :: Integer -> Integer -> [Integer]
powersOf2 num currPower
  | num < 2^currPower = []
  | num `mod` 2^currPower == 0 = currPower : powersOf2 num (currPower + 1)
  | otherwise = powersOf2 num (currPower + 1) 

ruler' :: Stream Integer
ruler' = streamMap evenlyDivision counting 

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = (Cons a (Cons b (interleaveStreams as bs)))

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) ruler'

-- Ex 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = createIntStream n 1
    where createIntStream n curr
            | n == curr = Cons 1 (streamRepeat 0) 
            | otherwise = Cons 0 (createIntStream n (curr + 1))
  negate ss = streamMap (*(-1)) ss
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0+b0) (a'+b')
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0*b0) ((streamMap (*a0) b') + (a'*b))

instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0 `div` b0) (streamMap (*(1`div`b0)) (a' - q*b'))
    where q = a / b

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2) -- Whoa!

-- Ex 7
data Matrix = Matrix { m11 :: Integer
                     , m12 :: Integer
                     , m21 :: Integer
                     , m22 :: Integer
                     } deriving (Show, Eq)

instance Num Matrix where
  (*) (Matrix m11 m12 m21 m22) (Matrix n11 n12 n21 n22) =
    Matrix (m11*n11 + m12*n21) (m11*n12 + m12*n22) (m21*n11 + m22*n21) (m21*n12 + m22*n22)

fib4 :: Integer -> Integer
fib4 n
  | n == 0 = 0
  | otherwise = m11 (f1 ^ n)
    where f1 = Matrix 1 1 1 0
