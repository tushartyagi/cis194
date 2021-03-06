{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
type Name = String
type Phone = String

data Employee = Employee { name :: Name
                         , phone :: Phone }
              deriving Show

createEmp name phone = Employee name phone

-- Ex 1
-- Functor instance for Parser
first :: (a->b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

-- Both `f` and `a` below are functions, and we are pretty sure about
-- the signature of a :: String -> Maybe (a', String)
-- To this Maybe (a', String), we need to fmap a function which will
-- work on the pair: (a', String) -- and that function is `first` which
-- applies the given function to the first of pair
instance Functor Parser where
  fmap f (Parser a) = Parser (fmap (first f) . a)

-- > runParser (fmap (*2) posInt) "12312"
-- Just (24624,"")

-- Ex 2
pureParser a = Parser f
  where
    f [] = Nothing
    f xs = Just (a, xs)

-- The trick here was to understand that the first parser will return a
-- function instead of a value which has been the norm till this point.
-- This function is then applied to the value that is produced by running
-- the second parser. 
instance Applicative Parser where
  pure a = pureParser a
  p1 <*> p2 = Parser f
    where f s = case runParser p1 s of
            Nothing -> Nothing
            Just (f', s') -> first f' <$> runParser p2 s' 

-- Ex 3
aParser :: Parser Char
aParser = satisfy (== 'a')

bParser :: Parser Char
bParser = satisfy (== 'b')

abParser :: Parser (Char, Char)  
abParser = (,) <$> aParser <*> bParser

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a b c -> [a,c]) <$> posInt <*> satisfy (== ' ') <*> posInt

-- Ex 4
-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = Parser f
    where f _ = Nothing
  p1 <|> p2 = Parser f
    where f s = runParser p1 s <|> runParser p2 s

-- Ex 5
intOrUppercase :: Parser ()
intOrUppercase = (const ()) <$> posInt <|> (const ()) <$> satisfy isUpper
