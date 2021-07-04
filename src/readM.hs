import Control.Monad.Reader


type Val = Int
type Var = String

data Exp = ValEx Val 
         | VarEx Var
         | SumEx Exp Exp
         deriving (Show)

-- Just crash if he variable is not in the environment  

newtype Env = E [(Var,Val)] deriving (Show)
emptyEnv :: Env
emptyEnv = E []
applyEnv :: Env -> Var -> Val
applyEnv (E []) _  = error "Variable not in environment"
applyEnv (E ((varx,valx):xs)) var
           | var == varx = valx
           | otherwise = applyEnv (E xs) var 
extendEnv x (E xs) = E (x:xs)
 
-- Evaluation

evalM :: Exp -> Reader Env Val
evalM (ValEx v) = return v
evalM (VarEx v) = do
                    env <- ask
                    return (applyEnv env v)
evalM (SumEx e1 e2) = do
                        n1 <- evalM e1
                        n2 <- evalM e2
                        return (n1 + n2)



 
