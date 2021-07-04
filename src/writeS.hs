{-
Simpler attempt !!

Want functions of Int -> Int, anotate them to Int -> (Int,String)
-}

newtype Writer a = W (a -> (a, String))

runW :: Writer a -> a -> (a, String)
runW (W f) = f 

return :: a -> Writer a
return x = W (\_ -> (x,""))

(>>=) :: (Writer a) -> (Writer a) -> (Writer a)
g >>=  f = W (\x -> let (y,s) = (runW g x)
                        (z,s') = (runW f y)
                    in
                      (z,s'++s))
