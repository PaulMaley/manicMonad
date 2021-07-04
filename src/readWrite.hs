{-
OK, used the Reader for Env and Writer for logging
Now, combine the two with a Monad Transformer ....
-}

import Control.Monad.Reader
import Control.Monad.Writer


type Val = Int
type Var = String

data Exp = ValEx Val 
         | VarEx Var
         | SumEx Exp Exp
         deriving (Show)

-- Just crash if he variable is not in the environment  

newtype Env = E [(Var,Val)] deriving (Show)

--newtype Writer a = W (a -> (a, String))

applyEnv :: Env -> Var -> Val
applyEnv (E []) _  = error "Variable not in environment"
applyEnv (E ((varx,valx):xs)) var
           | var == varx = valx
           | otherwise = applyEnv (E xs) var 
extendEnv x (E xs) = E (x:xs)
 
-- Evaluation
-- evalM will return a function :: Env -> Val for a given Exp.
-- We evaluate this function using runReader (as it is encapsulated
-- in a Reader monad.
 
evalM :: Exp -> Reader Env Val
evalM (ValEx v) = return v
evalM (VarEx v) = do
                    env <- ask
                    return (applyEnv env v)
evalM (SumEx e1 e2) = do
                        n1 <- evalM e1
                        n2 <- evalM e2
                        return (n1 + n2)

-- Up a gear
-- Make a monad stack to deal with the environment and logging 

eval :: WriterT String (Reader Env) Val -> Env -> (Val, String)
eval = (runReader . runWriterT)

evalRW :: Exp -> WriterT String (Reader Env) Val
evalRW (ValEx n) = WriterT (reader (\_ -> (n,"Val")))
evalRW (VarEx var) = WriterT (reader (\e -> (applyEnv e var,"Var")))
{-
evalRW (SumEx e1 e2) = do
                         (n1,l1) <- evalRW e1
                         (n2,l2) <- evalRW e2
                         return (n1+n2, l1 `mappend`l2)
-}





 
