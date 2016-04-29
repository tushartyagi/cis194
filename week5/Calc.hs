module Calc where

import Expr
import ExprT
import Parser

-- Ex 1

eval :: ExprT -> Integer
-- eval (Lit n) = n
-- eval (Add x y) = (eval x) + (eval y)
-- eval (Mul x y) = (eval x) * (eval y)

eval n = case n of
  Lit n   -> n
  Add x y -> (eval x) + (eval y)
  Mul x y -> (eval x) * (eval y)

-- Ex 2

parser = parseExp Lit Add Mul

evalStr :: String -> Maybe Integer
evalStr s = case parser s of
  Nothing -> Nothing
  Just e -> Just (eval e)

-- Ex 3

instance Expr ExprT where
  lit n = Lit n
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

-- Ex 4

instance Expr Integer where
  lit n = n
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit n
    | n <= 0 = False
    | otherwise = True
  add x y = x || y
  mul x y = x && y

-- I realised here that the MinMax/Mod7 will behave a lot like other
-- containers(?) like Maybe, Either etc. i.e. the value is inside the
-- type. Also, printing it on the console will print type name as well:
-- > MinMax 12
-- MinMax 12
-- > Mod7 8
-- Mod7 1
-- The following code pattern matches the thing and uses the
-- contained values.
newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit n = MinMax n
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7


-- Ex 5: In a separate file since the property names conflict


  
