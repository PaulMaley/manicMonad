{-
Start from scratch
-}

newtype B1 a = B1 (a,Integer) deriving (Show)

base :: a -> B1 a 
base x = B1 (x,101)

value :: B1 a -> a
value (B1 (x,_)) = x 

context :: B1 a -> Integer
context (B1 (_,n)) = n

-- Satisfies the Functor laws
--  fmap id == id 
--  (fmap f) . (fmap g) == fmap (f . g) 
instance Functor B1 where
  -- fmap f (B1 (x,n)) = B1 (f x,n) 
  fmap f x = B1 (f (value x), context x)

instance Applicative B1 where
  pure = base
  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) f x  = B1 ((value f) (value x), context x)

instance Monad B1 where
  -- return a -> m a 
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- Here is where we have to decide how to combine the contexts .... Monoids 
  (>>=) x f = let x' = f (value x) 
              in B1 (value x', (context x) + (context x'))
                
{-
OK ... Here is a monoid which has a "data" value 
c.f. the parsing Monads and Eval Mondas that have "function" values 
fmap, pure, (<*>), and (>>=) will need to be coded accordingly.
example ...... Exercise to the reader  
-}





