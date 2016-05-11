module Adder where

newtype Adder = Adder Integer { getAdder :: Integer}

instance Monoid Adder where
  mempty = 0
  mappend (Adder x) (Adder y) = Adder (x + y)
