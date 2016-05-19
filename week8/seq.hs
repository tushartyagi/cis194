mainWithDo = do
  putStrLn "You! Tell me your favorite number!"
  numStr <- getLine
  let num = read numStr :: Integer
  putStrLn ("Your fav number is half of " ++ show (num * 2))

mainWithoutDo =
  putStrLn "You! Tell me your favorite number!" >>
  getLine >>=
  (\n -> putStrLn ("Your fav number is half of " ++ show (parseIntAndDouble n)))

parseIntAndDouble :: String -> Integer
parseIntAndDouble s = num * 2
  where num = read s :: Integer

main = mainWithoutDo
