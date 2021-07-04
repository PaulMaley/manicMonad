{-
OK ... now to figure out the Writer Monad

Example - series of functions f_1(x), f_2(x), .. etc..
Chain them together to form a computation
Add a log to record the computations done

See here "Endo" ... Just what I'm doing 
https://kseo.github.io/posts/2017-01-21-writer-monad.html
-}
module WriteM where

newtype F a = F (a -> a) 

func :: F a -> (a -> a)
func (F f) = f 

{-
compose is a monoid, no?? ... or is the mappend function for a monoid ??
-}
compose :: F a -> F a -> F a
f `compose` g = F (\x -> (func f) ((func g) x)) 

inc = F (\x -> x+1)
dec = F (\x -> x-1)
sqr = F (\x -> x*x)


{-
The log file
-}

newtype Log = Log [String] deriving (Show)

log :: String -> (Log -> Log)
log s = \(Log ls) -> Log (s:ls)

emptyLog :: Log
emptyLog = Log []

linc = WriteM.log "inc"
ldec = WriteM.log "dec"
lsqr = WriteM.log "sqr"

{-
OK ... and the writer

I'm working with types (F a)
Want to add the context Log

Need to define return and bind
Writer a w. (a is the computation, w is the log)
-}

newtype Writer a = Writer (a,Log) deriving (Show)

runW :: (Writer a) -> (a,Log)
runW (Writer (x,l)) = (x,l)

returnW :: a -> Writer a
returnW x = Writer (x,emptyLog)

funcM :: (Writer (F a)) -> (a -> (a,Log))
funcM w = \x -> let (f,l) = runW w in
                  ((func f) x,l)




