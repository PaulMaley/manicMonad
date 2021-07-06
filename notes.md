# Monads and stuff 
Try to see how the Haskell standard types like Monads
can be used -- as they seem to be used by everybody !!

## Simple expression language with environment 
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

## Experiments 
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

## Stacking Monads 
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

## Round 2 
Want to chain together a series of functions `Int -> Int`
to get a final result.
However, a function *might* require a variable from the
environment.
We'll use the Reader to pass the environment through
the computation
Introduce the possibility of failure - when a 
variable is not found in the environment

Here is the code ... and it works. 

```
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
(>>=) :: (Maybe Int,Env) -> (Int -> Env -> Maybe Int) -> (Maybe Int,Env)
(>>=) (mx,env) f = case mx of
                     Nothing -> (Nothing, env)
                     Just x -> (f x env, env)

```

Testing in ghci:

``` 
Main> env = emptyEnv 
Main> f x env = Just (x+1) 
Main> g x env = Just (x*x+1) 
Main> (Just 1, env) >>= f 
Main> (>>=) = (Main.>>=)
Main> (Just 1, env) >>= f 
(Just 2,E [])
Main> (Just 1, env) >>= f >>= f
(Just 3,E [])
Main> (Just 1, env) >>= f >>= f >>= g
(Just 10,E [])
Main> h x env = case (valueOf "y" env) of Nothing -> Nothing; (Just y) -> Just (x*y)  
Main> (Just 1, env) >>= f >>= f >>= h
(Nothing,E [])
Main> (Just 1, (extendEnv ("y",1) emptyEnv)) >>= f >>= f >>= h
(Just 3,E [("y",1)])
Main> (Just 1, (extendEnv ("y",1) emptyEnv)) >>= f >>= f >>= h >>= f
(Just 4,E [("y",1)])
Main> (Just 1, (extendEnv ("y",1) emptyEnv)) >>= f >>= f >>= h >>= h
(Just 3,E [("y",1)])
Main> 
```

Now how to reimplement this with standard stuff ?

## Using Monadic feature of Maybe
This is in `readMaybe.hs`

```
-- Maybe is a Monad 
-- return (emptyEnv,0) >>= f 
-- gives 1 .... aha.
f :: (Env,Int) -> Maybe (Env,Int)
f (env, x) = Just (env, x+1)

g :: (Env,Int) -> Maybe (Env,Int)
g (env, x) = Just (env, x * x)

-- And a lookup
h :: (Env,Int) -> Maybe (Env,Int)
h (env, x) = case valueOf "y" env of
               Nothing -> Nothing
               Just y -> Just (env, x*y)
```
It uses `>>=` as defined by `Maybe` and works nicely.
(Heve `Env` can fail to find a value in the environment).

```
Main> (f (extendEnv ("y",4) emptyEnv ,0)) >>= g >>= h
Just (E [("y",4)],4)
Main> f (emptyEnv ,0)
Just (E [],1)
Main> (f (emptyEnv ,0)) >>= g
Just (E [],1)
Main> (f (emptyEnv ,0)) >>= g >>= h
Nothing
Main> (f (extendEnv ("y",4) emptyEnv ,0)) >>= g >>= h
Just (E [("y",4)],4)
Main> 
```




## Playing with `Reader` in `ghci`

Here there is no failure in lookup in the environment. Just trying
to understand the types.
```
Main> f =  reader (\env -> (\x -> x+1)) :: Reader Env (Int -> Int)
Main> g = reader (\env -> (\x -> x * (applyEnv  env "y"))) :: Reader Env (Int -> Int)
Main> (runReader g (E [("y",4)]))   2
8
Main> (runReader f (E []))   0
1
```

## Change of tack
Following some lectures from Uni Freiburg in Germany (Thiemann).
Got down this far ....

```
evalM :: Monad m => Term -> m Integer
evalM (Con n) = return n
evalM (Bin t op u) = (evalM t) >>= \v -> 
                     (evalM u) >>= \w -> 
                     return (sys op v w)
```

So this is a parametrized function.
The following 'work' straight out the bag:

```
Lib> evalM (Con 3)
3
Lib> evalM (Con 3) :: Maybe Integer
Just 3
Lib> evalM (Con 3) :: Either String Integer
Right 3

``` 

Because bind and return are defined -- they are Monad instances.
However, neither will catch divide by zero because it's not coded
in the above function.

Implemented `Monad Exception`

```
Lib> evalM (Bin (Con 3) Add (Bin (Con 10) Div (Con 2)))  :: Exception Integer
Return 8
```

So, using `fail` gave a *generic* way to catch the divide by zero
for the different Monads. But `fail` is no longer in the monad class.
So now eval `(Bin t op u)` would need to be implemented on a monad
by monad basis ..... ok.
  
What's more the next three implementations *all* reimplement the `eval`
function. In other words there differences are not abstracted away by
the Monad type. Question then: what is abstracted across the different 
types?
 
Implement Trace :
```
evalMT :: Term -> Trace Integer
evalMT (Con n) = output (trace (Con n) n) >> return n
evalMT (Bin t op u) = evalMT t >>= \v -> 
                      evalMT u >>= \w ->
                      let r = sys op v w 
                      in output (trace (Bin t op u) r) >> return r
``` 

Works nicely:
```
Lib> evalMT (Bin (Con 3) Add (Bin (Con 10) Div (Con 2))) 
Trace (8,"eval (Con 3)=3\neval (Con 10)=10\neval (Con 2)=2\neval (Bin (Con 10) Div (Con 2))=5\neval (Bin (Con 3) Add (Bin (Con 10) Div (Con 2)))=8\n")
Lib>
```

## Using the State Monad
This is a f&%king rabbit hole again. 
Short answer ... add `mtl` to the `packages.yaml` file and then use
the Hackage documentation under mtl. A nightmare to get this far !!

On the other hand it works nicely.

```
-- Redo Count using State ....
-- define with ' so as to have both at the same time.
-- This means rewriting evalMC with 's. This wouldn't be 
-- necessary otherwise
type Count' a = State Int a

incr' :: Count' ()
incr' = state (\i -> ((),i+1)) 

evalMC' :: Term -> Count' Integer
evalMC' (Con n) = return n
evalMC' (Bin t op u) = evalMC' t >>= \v ->
                       evalMC' u >>= \w ->
                       incr' >>
                       return (sys op v w) 
```

And in `ghci`
```
Lib> ex1 = (Bin (Bin (Con 2) Add (Con 0)) Add (Bin (Con 10) Div (Con 2)))
Lib>  runState (evalMC' ex1) 0
(7,3) 
```




