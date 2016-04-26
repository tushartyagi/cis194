toDigitsRev :: Integer -> [Integer]
toDigitsRev d | d <= 0 = []
              | otherwise = lastDigit : toDigitsRev remainingDigits
  where lastDigit = d `mod` 10
        remainingDigits = d `div` 10


toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [if even idx
                               then 2 * val
                               else val
                              | (idx,val) <- zip [1..] (reverse xs)]

sumDigits :: [Integer] -> Integer 
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate xs
  | totalSum `rem` 10 == 0 = True
  | otherwise = False
  where totalSum = sumDigits . doubleEveryOther . toDigits $ xs
