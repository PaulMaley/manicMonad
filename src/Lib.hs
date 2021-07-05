{-
Found some lectures on exactly what I want to do ... 
just going to follow them and implement the code here.

Lectures by P. Thiemann, Univ Freiburg 

(Need to write a parser for this ...)
Good to go over this again ... clearer this time
-}
module Lib where

import Control.Monad

data Term = Con Integer
          | Bin Term Op Term
            deriving (Eq, Show)

data Op = Add | Sub | Div | Mul
          deriving (Eq, Show)

{- A Parser -}
type Parser token result = [token] -> [(result,[token])]

pempty :: Parser t r
pempty ts = []

succeed :: r -> Parser t r
succeed r ts = [(r,ts)]

satisfy :: (t -> Bool) -> Parser t t
satisfy p (t:ts) | (p t) = [(t,ts)] -- = succeed t ts
                 | otherwise  = []
satisfy p _ = []

-- Alternative to avoid tokens in the result
msatisfy :: (t -> Maybe a) -> Parser t a
msatisfy f (t:ts) = case (f t) of
                      Just a -> succeed a ts   --[(r,ts)]
                      Nothing -> pempty (t:ts) -- []
msatisfy f _ = []

lit :: Eq t => t -> Parser t t
lit t = satisfy (\t' -> t'==t)
   
palt :: Parser t r -> Parser t r -> Parser t r
palt p1 p2 ts = p1 ts ++ p2 ts

-- This one is hard ... don't understand it 
pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
pseq p1 p2 ts = 
  let srs = p1 ts  -- :: [(s->r,[t])]
      g (srf, ts1) = map (h srf) $ p2 ts1 -- [(s, [t])]
      h srf (s, ts2) = (srf s, ts2)
      rs = map g srs -- [[(r, [t])]]
  in concat rs

pmap :: (s -> r) -> Parser t s -> Parser t r
pmap f p ts = map (\(r, e) -> (f r, e)) $ p ts 
 
{- examples -}
-- Not sure what this is supposed to parse ...
-- Supposed to recognize ONLY "ab"
pex2 = pseq (pmap (const (const ())) (lit 'a')) (lit 'b')

pex3 = pmap (const ()) (lit 'c')
pex4 = palt pex2 pex3

-- A -> aA
-- A -> aB
-- B -> b

pA :: Parser Char String
pA = palt (pseq (pmap (:)(lit 'a')) pA)
          (pseq (pmap (:) (lit 'a')) pB)  
pB :: Parser Char String
pB = pmap return $ lit 'b'

{- OK, skip the parsing stuff and come back to the main -}

-- Evaluator

eval :: Term -> Integer 
eval (Con n) = n
eval (Bin t op u) = sys op (eval t) (eval u)

sys Add = (+)
sys Sub = (-)
sys Mul = (*)
sys Div = div

-- OK that works

-- Add error handling

data Exception a = Raise String 
                 | Return a
                 deriving (Show)
 
evalE :: Term -> Exception Integer 
evalE (Con n) = Return n
evalE (Bin t op u) = case evalE t of
                       Raise s -> Raise s           -- t screwed up: prop error
                       Return v -> case evalE u of
                         Raise s -> Raise s         -- u screwed up ...
                         Return w -> 
                           if(op==Div && w==0) 
                           then 
                             Raise "Divide by zero"
                           else
                             Return (sys op v w)
-- OK that works too

-- Add trace

newtype Trace a = Trace (a,String) deriving (Show)

evalT :: Term -> Trace Integer
evalT (Con n) = Trace (n, trace (Con n) n)   -- avoid e@(Con n) notation
evalT (Bin t op u) = 
  let Trace (v,sv) = evalT t
      Trace (w,sw) = evalT u
      r = sys op v w
  in
    Trace (r, sv++sw++ trace (Bin t op u) r)

trace t n = "eval (" ++ show t ++ ")=" ++ show n ++ "\n"

-- OK

-- Add Count 

newtype Count a = Count {exCount :: Int ->(a,Int)}

evalC :: Term -> Count Integer
evalC (Con n) = Count (\i -> (n,i))
evalC (Bin t op u) = Count (\i -> let (v,j) = exCount (evalC t) i
                                      (w,k) = exCount (evalC u) j
                                  in                                     
                                    (sys op v w, k+1))


-- Next step ... Monads again
-- Remember m :: * -> *. Type constructor

evalM :: Monad m => Term -> m Integer
evalM (Con n) = return n
evalM (Bin t op u) = (evalM t) >>= \v -> 
                     (evalM u) >>= \w -> 
                     return (sys op v w)
-- Alternative with do:
-- do v <- evalM t
--    w <- evalM u
--    return (sys op v w)

-- Using the Exception datatype defined above
-- Here need to first instantiate bind and return
-- fail is no longer in Monad ... differs from course
-- How to define a Monad more directly 
instance Functor Exception where
--fmap :: (a->b) -> f a -> f b
  fmap f mx = case mx of 
                Raise s -> Raise s
                Return x -> Return (f x) 

instance Applicative Exception where
  pure = Return
--(<*>) :: f (a->b) -> f a -> f b    
  (<*>) (Return f) mx = case mx of
                             Raise s -> Raise s
                             Return x -> Return (f x) 
  (<*>) (Raise s) _ = Raise s

instance Monad Exception where
  return = Return
  (>>=) mx f = case mx of 
                 Raise s -> Raise s
                 Return x -> f x    -- f is of type (a -> m a) 
                                    -- no need for return

-- an instance << evalM :: Exception Integer >> should work
-- out of the box too - bind and retrun defined.
-- Need to modify the eval to catch the div by zero.

-- Trace monad
instance Functor Trace where
--fmap :: (a -> b) -> f a -> f b
  fmap f (Trace (a,s)) = Trace (f a, s) 

instance Applicative Trace where
  pure = return 
--(<*>) :: f (a->b) -> f a -> f b
  (<*>) mf mx = let Trace (f,s) = mf 
                    Trace (x,s') = mx
                in Trace (f x, s ++ s')

instance Monad Trace where 
  return a = Trace (a, "") 
--(>>=) :: m a -> (a -> m b) -> m b  
  (>>=) (Trace (a,s)) f = let Trace (b, s') = f a
                          in Trace (b, s ++ s')  

--What's this for ... puts a string into an empty Trace
output :: String -> Trace ()
output s = Trace ((),s)

-- And reimplement the evaluator
-- Evaluation of (Con n) ... trace produces a string
-- output sticks it into a Trace value and then >>
-- uses >>= to add the string to the Trace obect returned
-- by return whuch has an empty string !!!!!
-- Trace (n, trace (Con n) n) is equivalent 

evalMT :: Term -> Trace Integer
evalMT (Con n) = output (trace (Con n) n) >> return n
evalMT (Bin t op u) = evalMT t >>= \v -> 
                      evalMT u >>= \w ->
                      let r = sys op v w 
                      in output (trace (Bin t op u) r) >> return r













