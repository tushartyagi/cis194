module Golf where

-- Exercise 1

skips :: [a] -> [[a]]
skips [] = []
skips xs = [nth xs n | n <- [1..(length xs)]]
{-
Previously I was using
skips xs = [elem | n <- [1..(length xs)], elem <- nth xs n]
which resulted in the elem-s being concatenated and therefore
giving me a single list of elements instead of a list of lists.
-}

nth :: (Integral n) => [a] -> n -> [a]
nth [] _ = []
nth xs n = [elem | (idx, elem) <- zip [1..] xs, idx `mod` n == 0]

