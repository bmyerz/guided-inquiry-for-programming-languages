
{- a language for expressions that includes values, variables, lambdas, and lambda application -}
{- this data type definition gives the (abstract) syntax of the language -}
data Expr = Val Int | Lambda Expr Expr | Var String | Apply Expr Expr | Add Expr Expr
  deriving Show

{- Example usage -}

{- The Haskell program
 (\x -> x + 1) 6
would analogously be written as
-}
e1 = Apply 
        (Lambda 
            (Var "x") 
            (Add 
                (Var "x") 
                (Val 1))) 
        (Val 6)

{- The Haskell program
(\y z -> y + z) 4 (5+1)
can be equivalently expressed as 
(\y -> (\z -> y + z) 4) (5 + 1)

and then analogously be written as
-}
e2 = Apply 
        (Lambda 
            (Var "y") 
            (Apply 
                (Lambda 
                    (Var "z") 
                    (Add 
                        (Var "y") 
                        (Var "z"))) 
                (Val 4))) 
        (Add 
            (Val 5) 
            (Val 1))

{- The Haskell program
(\a b -> (\c -> (c + 1) + 2) (a + b)) 9 7
can be equivalently expressed as 
<fill in answer>
and then analogously be written as
-}
e3 = undefined

-- the interpreter for Expr 
-- defines the semantics of the language
interpret :: Expr -> Int
interpret e = iHelper e []

iHelper :: Expr -> [(String,Int)] -> Int
iHelper (Val i) _ = i
iHelper (Var name) assignments = find' assignments name
iHelper (Add e1 e2) assignments = (iHelper e1 assignments) + (iHelper e2 assignments)
iHelper (Apply (Lambda (Var name) le) ae) assignments = let ai = iHelper ae assignments in
                                                          iHelper le ((name, ai):assignments)

find' :: [(String, Int)] -> String -> Int
find' (kv:kvs) k = if (fst kv) == k then (snd kv) else find' kvs k  

{- These programs attempt to solve the problem of
adding the integers 9 and 10 -}
e4 = Add (Val 9) (Val 19)
 
e5 = Apply (
           Lambda (Var "x")
                  (Apply (
                         Lambda (Var "y")
                                (Add
                                  (Val 10)
                                  (Var "y")))
                         (Val 10)
                   )
             )
            (Val 9)

e6 = Apply (
           Lambda (Var "x")
                  (Apply (
                         Lambda (Var "x")
                                (Add
                                  (Var "x")
                                  (Var "x")))
                         (Val 10)
                   )
             )
            (Val 9)

{- A slightly fancier version of the Maybe data type -}
data Exceptional x = Failure String | Success x
  deriving Show

instance Functor Exceptional where
  fmap f (Success x) = Success (f x)
  fmap f (Failure s) = Failure s
 
instance Applicative Exceptional where
  pure x = Success x

  (Failure s) <*> ex = Failure s
  (Success f) <*> ex = fmap f ex

instance Monad Exceptional where
  (Failure s) >>= f = Failure s
  (Success x) >>= f = f x

-- an example of using Exceptional
iOnlyAdd1To10 :: Int -> Exceptional Int
iOnlyAdd1To10 x = if x < 10 then Failure "Too small..."
                  else if x > 10 then Failure "Too big..."
                       else Success (x + 1)

