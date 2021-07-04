import Control.Monad 

type Val = Int
type Var = String

data Exp = ValEx Val 
         | VarEx Var
         | SumEx Exp Exp
         deriving (Show)

-- Just crash if he variable is not in the environment  

newtype Env = E [(Var,Val)] deriving (Show)
applyEnv :: Env -> Var -> Val
applyEnv (E []) _  = error "Variable not in environment"
applyEnv (E ((varx,valx):xs)) var
           | var == varx = valx
           | otherwise = applyEnv (E xs) var 
extendEnv x (E xs) = E (x:xs)
 
-- Evaluation

eval :: Exp -> Env -> Val 
eval (ValEx n) _ = n
eval (VarEx v) env = applyEnv env v
eval (SumEx e1 e2) env = (eval e1 env) + (eval e2 env)

-- With a Reader monad ...
-- The Env is the context 
-- Want something like eval' :: Exp -> Val (implicit env) ??

data Reader e a = Reader (e -> a) 

runR :: Reader e a -> e -> a
runR (Reader f) e = f e

--instance Functor (Reader env) where
--  fmap f x = 

--instance Monad (Reader env) where 
return :: a -> (Reader e a)
return x = Reader (\_ -> x) 

(>>=)  :: (Reader e a) -> (a -> Reader e b) -> (Reader e b)
g >>= f = Reader (\e -> runR (f (runR g e)) e) 

{-
Notes ... this works 
*Main> r var = Reader (\e -> applyEnv e var)
*Main> r' =  r "y" 
*Main> runR r'  (E [("y",3)])
3

-}

 
