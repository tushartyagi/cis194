module Party where

import Employee
import Data.Tree

-- Ex 1

-- Adds the employee to the guest list blindly
-- Will not check for duplication
-- Will update the STFU score
glCons :: Employee -> GuestList -> GuestList
glCons e (GL eList fun) = GL (e:eList) (empFun e + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL el1 f1) (GL el2 f2) = GL (el1 ++ el2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 < gl2
                  then gl2
                  else gl1

-- Testing Data
gl0 = GL [] 0
e = Emp { empName = "Foo", empFun = 12}
f = Emp { empName = "Bar", empFun = 15}
g = Emp { empName = "Baz", empFun = 18} 
gl1 = glCons e gl0
gl2 = glCons f gl0
gl3 = glCons g gl0 
gl123 = gl1 `mappend` gl2 `mappend` gl3


-- Ex 2
-- A fold function should take a accumulator function, a seed value,
-- a foldable type, and returns the folded value of type seed.
-- E.g.:  foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- Therefore, the signature is missing both the accumulator function
-- and the seed value.
treeFold :: (a -> b -> b) -> b -> Tree a -> b 
treeFold = undefined

