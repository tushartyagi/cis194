-- From the timestamp, this was written on 26/10/2015

charToString :: c -> [c] 
charToString c = [c]

toDigits :: Integer -> [Integer]
toDigits number
  | number <= 0 = []
  | otherwise = map (read::String->Integer) digitCharacters
  where digitCharacters = map charToString stringifiedDigits
        stringifiedDigits = show number

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:ys) = x : (2*y : doubleEveryOtherFromLeft ys)

sumCharacters :: String -> Integer
sumCharacters [] = 0
sumCharacters (x:xs) = read (charToString x) + sumCharacters xs

sumDigits :: [Integer] -> Integer
sumDigits =  sumCharacters . foldl1 (++) . map show 

validate :: Integer -> Bool
validate xs
  | totalSum `rem` 10 == 0 = True
  | otherwise = False
  where totalSum = sumDigits . doubleEveryOther . toDigits $ xs
