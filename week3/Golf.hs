module Golf where

-- Exercise 1

skips :: [a] -> [[a]]
skips [] = []
skips xs = [nth n xs | n <- [1..(length xs)]]

nth :: (Integral n) => n -> [a] -> [a]
nth _ [] = []
nth n xs = [elem | (idx, elem) <- zip [1..] xs, idx `mod` n == 0]

{-
Previously I was using
skips xs = [elem | n <- [1..(length xs)], elem <- nth xs n]
which resulted in the elem-s being concatenated and therefore
giving me a single list of elements instead of a list of lists.
-}
  
-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs = [m | [l,m,n] <- neighbours 3 xs, m > l && m > n]

neighbours :: Int -> [a] ->  [[a]]
neighbours n xs
  | length xs < n = []
  | otherwise = [take n xs] ++ neighbours n (tail xs)


localMaxima' :: [Integer] -> [Integer]
localMaxima' (x:y:z:zs)
  | y > x && y > z = y : localMaxima'(y:z:zs)
  | otherwise = localMaxima'(y:z:zs)
localMaxima' _ = []

-- Exercise 3

histogram :: [Integer] -> String
histogram xs = unlines $ starLines ++ lineBreak ++ numbers
  where lineBreak = ["=========="]
        numbers   = ["0123456789"]
        starLines = reverse $ stars
        stars = prepareStars . frequency $ xs

-- Creates a frequency list of numbers 1 to 9 in the input
-- e.g. [1,1,1,5] will give [0,3,0,0,0,1,0,0,0,0]
frequency :: (Enum a, Eq a, Num a) => [a] -> [Int]
frequency xs = [length (filter (\x -> x == n) xs) | n <- [0..9]]

-- prepares the histogram string.
-- This will have m elements (where m is the highest number in the list)
-- e.g. [0,3,0,0,0,1,0,0,0,0] which shows that 1 comes three times
-- and 5 comes once, and will have 3 lines (in order to show it along 3
-- lines vertically).
-- [0,3,0,0,0,1,0,0,0,0] -> [" *   *    "," *        "," *        "]
prepareStars :: (Eq a, Num a, Ord a) => [a] -> [[Char]]
prepareStars xs
  | xs == replicate 10 0 = []
  | otherwise = [if n > 0 then '*' else ' ' | n <- xs]
    : prepareStars (step xs)

-- Subtract each element (except 0) in the list by 1. If the list
-- is made up entirely of zeros, then we are done.
step xs = map safeSubtract1 xs
  where safeSubtract1 = (\x -> if x == 0 then 0 else x - 1)
        
sample1 = [1,1,1,5]
sample2 = [1,4,5,4,6,6,3,4,2,4,9]
