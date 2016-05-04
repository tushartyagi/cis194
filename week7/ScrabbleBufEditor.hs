module Main where

import ScrabbleBuffer
import Editor

-- I am unable to get this working. Currently I am not able to run the
-- fromString function from ScrabbleBuffer instance.
-- The editor is correctly working in ScrabbleBuffer by running:
-- `runhaskell ScrabbleBuffer.hs`

main = runEditor editor fs . unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

-- Removing the ambiguity means that i will have to import JoinList, Sized,
-- and Scrabble in this module which seems to me a very unclean thing since
-- the ScrabbleEditor is supposed to be the interface for that and it already
-- has those required modules. Will come back to this sometime later.
