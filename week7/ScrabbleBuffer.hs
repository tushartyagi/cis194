{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module ScrabbleBuffer where

import Data.Monoid

import Buffer
import JoinList
import Scrabble
import Sized
import Editor

instance Buffer (JoinList (Score, Size) String) where
  toString Empty           = "" 
  toString (Single _ s)    = s
  toString (Append _ l r)  = toString l ++ toString r 

  fromString ls = foldr createJL Empty (lines ls)
    where createJL curr acc = acc +++ createSingle curr 

  line                     = indexJ  

  replaceLine n l b = takeJ (n - 1) b +++ createSingle l +++ dropJ n b
          
  numLines Empty = 0
  numLines (Single _ _) = 1
  numLines (Append _ l r) = numLines l + numLines r

  value = sizeJL

createSingle l = Single (score, size) l
  where score = scoreString l
        size  = Size 1

main = runEditor editor (fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String)

