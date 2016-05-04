{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Char

newtype Score = Score Int 
              deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score s) = s

instance Monoid Score where
  mempty  = Score 0
  mappend = (+) 

score :: Char -> Score
score char
  | c `elem` "aeilnorstu" = Score 1
  | c `elem` "dg"         = Score 2
  | c `elem` "bcmp"       = Score 3
  | c `elem` "fhvwy"      = Score 4
  | c `elem` "k"          = Score 5
  | c `elem` "jx"         = Score 8
  | c `elem` "qz"         = Score 10
  | otherwise = Score 0
  where c = toLower char
        
scoreString :: String -> Score
scoreString []     = Score 0
scoreString (x:xs) = score x + scoreString xs
