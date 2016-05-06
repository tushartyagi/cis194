module Party where

import Employee
import Data.Tree
import Data.List(sort, intersperse)

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
treeFold :: (a -> [b] -> b) -> Tree a -> b 
treeFold f (Node r xs) = f r (map (treeFold f) xs)


-- Ex 3
-- The sublists do not have to be recursively generated in *this* function,
-- these will be generated later. This flawed understanding took up a lot of my -- time and brainpower.
-- The following solution made it quite clear that we will have to use the
-- treeFold to do the recursion.
-- An elegent solution, taken from: https://github.com/evansb/cis194-hw
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, withoutBoss)
  -- map the moreFun function to all the subdepartments, which will pick the
  -- moreFun gl from each subdepartment. This has all the subdepartments, and
  -- since may include the sub-bosses, the current boss is excluded.
  -- withBoss, on the other hand will have to pickup only the second list
  -- from the pair, because the sub-bosses and boss will not gel well.
  where withoutBoss = mconcat (map (uncurry moreFun) gls)
        withBoss    = glCons boss (mconcat (map snd gls))

-- Ex 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel 

-- Ex 5
instance Ord Employee where
  compare e1 e2 = empName e1 `compare` empName e2

-- Returns a list of sorted empList and fun
getEmployeesAndFun :: String -> ([Name], Integer)
getEmployeesAndFun empString = (sortedEmpList, fun)
  where empTree = read empString :: Tree Employee
        gl@(GL empList fun) = maxFun empTree
        sortedEmpList = map (empName) (sort empList)

formatEmps :: String -> String
formatEmps empString = show fun ++ "\n" ++ separatedEmps
  where (empNames, fun) = getEmployeesAndFun empString
        separatedEmps = concat $ intersperse "\n" empNames

main =
  readFile "company.txt" >>= (\empString -> putStrLn $ formatEmps empString)
