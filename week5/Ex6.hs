{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Data.Map as M
import Expr
import Parser

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit n = Lit n
  add x y = Add x y
  mul x y = Mul x y

instance HasVars VarExprT where
  var s = Var s 

-- this class instance works with a function that takes in a
-- map of string and integer and returns the value associated
-- with that key.
-- The implementation below is `exp $ M.fromLists vs` which
-- for input of `var "x"` will be converted to:
-- M.lookup "x" $ M.fromList vs
-- So we are, quite amazingly, are getting the value associated
-- with the given value from the map.
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

-- Similarly, this will again take in the same String, Integer map.
-- In case of Lit n, ignore the incoming map and return the value n.
-- Here, we are only interested in the nested `var` values which may
-- come up in `add` or `mul`.
-- Also, since the return type now is Maybe a, we return the values
-- accordingly.
-- This took me good 40-45 minutes of thinking time, and at the end
-- I claim that pen and paper are the best thinking tools. It was
-- only when I wrote down the input and output of these functions was
-- I able to get a breakthrough.
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n   = (\_ -> Just n)  
  add x y = \vs -> case (x vs, y vs) of    
    (Just n, Just m) -> Just (n + m)
    otherwise -> Nothing
  mul x y = \vs -> case (x vs, y vs) of
    (Just n, Just m) -> Just (n * m)
    otherwise -> Nothing

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

