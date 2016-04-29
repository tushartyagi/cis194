module StackVM (StackVal(..), StackExp(..), Stack, Program, stackVM) where

-- Values that may appear in the stack. Such a value will also be
-- returned by the stackVM program execution function.
data StackVal = IVal Integer | BVal Bool | Void deriving Show

-- The various expressions our VM understands.
data StackExp = PushI Integer
              | PushB Bool
              | Add
              | Mul
              | And
              | Or
                deriving Show

type Stack   = [StackVal]
type Program = [StackExp]

-- Execute the given program. Returns either an error message or the
-- value on top of the stack after execution.
stackVM :: Program -> Either String StackVal
stackVM = execute [] -- Start with an empty stack.

errType :: String -> Either String a
errType op = Left $ "Encountered '" ++ op ++ "' opcode with ill-typed stack."

errUnderflow :: String -> Either String a
errUnderflow op = Left $ "Stack underflow with '" ++ op ++ "' opcode."

-- Execute a program against a given stack.
execute :: Stack -> Program -> Either String StackVal
-- If the Program stack is empty, then return whatever is in the
-- application stack (or Void)
execute [] []                               = Right Void
execute (s:_) []                            = Right s

-- If the Program stack has values, pull the first value to the app stack
-- and execute the operation on the application.
execute s (PushI x : xs)                    = execute (IVal x : s) xs
execute s (PushB x : xs)                    = execute (BVal x : s) xs

-- If there are 2 values on the top of app stack and Add operation on the
-- top of Program stack, then add the values and store the result on the
-- app stack. Any other combination should not work.
execute (IVal s1 : IVal s2 : ss) (Add : xs) = execute (s':ss) xs
    where s' = IVal (s1 + s2)
execute (_:_:_) (Add:_)                     = errType "Add"
execute _ (Add:_)                           = errUnderflow "Add"

-- Ditto for Multiplication operation.
execute (IVal s1:IVal s2:ss) (Mul : xs)     = execute (s':ss) xs
    where s' = IVal (s1 * s2)
execute (_:_:_) (Mul:_)                     = errType "Mul"
execute _ (Mul:_)                           = errUnderflow "Mul"

-- And for Bools.
execute (BVal s1:BVal s2:ss) (And : xs)     = execute (s':ss) xs
    where s' = BVal (s1 && s2)
execute (_:_:_) (And:_)                     = errType "And"
execute _ (And:_)                           = errUnderflow "And"

execute (BVal s1 : BVal s2 : ss) (Or : xs)  = execute (s':ss) xs
    where s' = BVal (s1 || s2)
execute (_:_:_) (Or:_)                      = errType "Or"
execute _ (Or:_)                            = errUnderflow "Or"

test = stackVM [PushI 3, PushI 5, Add]
