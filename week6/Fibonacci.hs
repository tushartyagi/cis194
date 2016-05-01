module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- Each operation takes O(n) time
fib2 :: Integer -> Integer
fib2 n = fib2' 0 1 n
  where fib2' acc next n = if n == 0 then next
                           else fib2' next (acc + next) (n - 1)

fibs2 :: [Integer]
fibs2 = 0 : [fib2 n | n <- [0..]] 
