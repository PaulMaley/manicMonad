# Monads and stuff #
Try to see how the Haskell standard types like Monads
can be used -- as they seem to be used by everybody !!

## Simple expression language with environment ##
Language specification:

```
Exp ::= Int
      | Id
      | Sum Exp Exp
```

Can't get much simpler.
```
eval Int -> an integer
eval Id -> integer value of Id in environment
eval Sum e1 e2 -> (eval e1) + (eval e2)
```


The `Reader` class seems to be the way to go .....

ok ... some progress ... but a lot left to understand.  
I'm following: [EECS 662 at KU](http://ku-sldg.github.io/plih//funs/5-Reader-Monad.html).

My code (No `Monad` instance, just function definitions):
```
type Var = String

data Exp = ValEx Val
         | VarEx Var
         | SumEx Exp Exp
         deriving (Show)

-- Just crash if he variable is not in the environment

newtype Env = E [(Var,Val)]
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

data Reader e a = Reader (e -> a)

runR :: Reader e a -> e -> a
runR (Reader f) e = f e

return :: a -> (Reader e a)
return x = Reader (\_ -> x)

(>>=)  :: (Reader e a) -> (a -> Reader e b) -> (Reader e b)
g >>= f = Reader (\e -> runR (f (runR g e)) e)
```

## Experiments ##
```
*Main> e0 = E []
*Main> return = Main.return
*Main> (>>=) = (Main.>>=)
*Main> runR  (return 5) e0
5
*Main> e1 =  E [("x",4)]
*Main> add5tox = Reader (\e -> eval (SumEx (ValEx 5) (VarEx "x")) e)
*Main> runR add5tox e1
9
*Main> :t add5tox
add5tox :: Reader Env Val
*Main> lookup var = Reader (\e -> applyEnv e var)
*Main> lx = lookup "x"
*Main> runR lx e1
4
-- With Bind
*Main> add5 = \val -> return (val +5)  
*Main> runR ((return 5) >>= add5) e0
10
*Main> runR ((return 5) >>= add5 >>= add5) e0
15
*Main> runR ((lookup "x") >>= add5 >>= add5) e1
14
-- Extend the environment
*Main> extend x (Reader f) = Reader (\e -> f (extendEnv x e))
*Main> runR (extend ("x",1) (lookup "x")) e0
1
```

## Stacking Monads ##
OK, things getting a bit wild ...  
Added the code:
```
evalRW :: Exp -> WriterT String (Reader Env) Val
evalRW (ValEx v) = return v
```
and execute:
```
*Main> e0 = E []
*Main>  runReader (runWriterT (evalRW (ValEx 4))) e0
(4,"")
*Main>
```
Amazing! WTF. OK, so String is [Char] so shouldn't be surprised
that `mempty` (?) give `""`  
OK, first problem -- how to log the expression type ?

This is hard ... just getting any god-damned thing to run is hard.
Finally ... the following does something !!
```
*>w1 = WriterT (reader (\_ -> (1,"Val")))
*>:t w1
w1 :: (MonadReader r m, Num a) => WriterT [Char] m a
*> (runReader . runWriterT) w1 []
(1,"Val")
```
