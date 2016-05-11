module Employee where

type Name = String
type Phone = String

data Employee = Employee { name :: Name }
--                         , phone :: Phone }
              deriving Show

createEmp :: Name -> Employee
createEmp name = Employee name

-- The lift operation -- or how the functor would have
-- been implemented.
f :: Maybe Name -> Maybe Employee
f Nothing = Nothing
f (Just name) = Just (createEmp name)

-- fmap lifts the createEmployee function so that it
-- can be applied to Maybe Employee
-- > fmap createEmployee (Just "Tushar")
-- Just (Employee {name = "tushar"})

data Either1 a b = Left1 a | Right1 b
                   deriving Show

instance Functor (Either1 a) where
  fmap _ (Left1 b) = Left1 b
  fmap f (Right1 b) = Right1 (f b)
  
-- Implementing the questions at Typeclassopedia


-- instance Functor ((->) e) where
--   -- fmap :: (a -> b) -> (e -> a) -> (e -> b)
--   fmap = (.)

-- instance Functor ((,) e) where
--   -- fmap :: (a -> b) -> (e , a) -> (e , b)
--   fmap f (e,a) = (e,f a)

data Pair a = Pair a a
            deriving Show

-- Satisfies both the functor laws
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b) 

data Maybe' a = Just' a | Nothing'
              deriving Show

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*> _ = Nothing'
  _ <*> Nothing' = Nothing'
  Just' f <*> Just' x = Just' (f x)
