{-
So, in a bid to better understand .... a new program

Spec:
Want to chain together a series of functions Int -> Int
to get a final result.
However, a function *might* require a variable from the
environment.
We'll use the Reader to pass the environment through
the computation
Introduce the possibility of failure - when a 
variable is not found in the environment  

Now try to do this using the Reader and Maybe Monads
-}

import Control.Monad.Reader


type Val = Int
type Var = String

data Exp = ValEx Val 
         | VarEx Var
         | SumEx Exp Exp
         deriving (Show)

-- Environment - step 1 (recyle from before)
-- Just crash if he variable is not in the environment  

newtype Env = E [(Var,Val)] deriving (Show)

emptyEnv :: Env
emptyEnv = E []

applyEnv :: Env -> Var -> Maybe Val
applyEnv (E []) _  = Nothing
applyEnv (E ((varx,valx):xs)) var
           | var == varx = Just valx
           | otherwise = applyEnv (E xs) var 
extendEnv x (E xs) = E (x:xs)

valueOf :: String -> Env -> Maybe Int
valueOf x env = applyEnv env x
 
-- First implement what I want .. then see how to build it from 
-- Monads and stuff ...

-- Want this:
{-
(>>=) :: (Maybe Int,Env) -> (Int -> Env -> Maybe Int) -> (Maybe Int,Env)
(>>=) (mx,env) f = case mx of
                     Nothing -> (Nothing, env)
                     Just x -> (f x env, env)
-}  

-- Want: Reader Env Func
{-
newtype Func = F (Int -> Int)

calc :: Func -> Int -> Int
calc (F f) x = f x

f :: Reader Env Func
f = return (F (\x -> x+1))

g :: Reader Env Func
g = return (F (\x -> x*x))
-}

-- Maybe is a Monad 
-- return (emptyEnv,0) >>= f 
-- gives 1 .... aha.
f :: (Env,Int) -> Maybe (Env,Int)
f (env, x) = Just (env, x+1)

g :: (Env,Int) -> Maybe (Env,Int)
g (env, x) = Just (env, x*x)

-- And a lookup
h :: (Env,Int) -> Maybe (Env,Int)
h (env, x) = case valueOf "y" env of
               Nothing -> Nothing
               Just y -> Just (env, x*y)
 
