{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char
import Lecture

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
  
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum) 

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseInt :: Parser Integer
parseInt =  (spaces *> posInt)

parseIdent :: Parser Ident 
parseIdent = (spaces *> ident)

parseAtom :: Parser Atom
-- Using functor to transform `Parser Int` and `Parser Ident` to `Parser Atom`
parseAtom = (\i -> N i) <$> parseInt <|> (\i -> I i) <$> parseIdent

parseLBracket, parseRBracket :: Parser Char
parseLBracket = char '('
parseRBracket = char ')'

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

parseSStar = (parseLBracket *> oneOrMore parseSExpr <* parseRBracket)

parseSExpr :: Parser SExpr
parseSExpr = (\a -> A a) <$> trim parseAtom <|> (\s -> Comb s) <$> trim parseSStar  
