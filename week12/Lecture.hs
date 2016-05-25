module Lecture where

import Control.Applicative (many)

check :: Int -> Maybe Int
check n | n < 10 = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n = Just $ n `div` 2
        | otherwise = Nothing

add1or2 :: Int -> [Int]
add1or2 x = [x + 1, x + 2]
