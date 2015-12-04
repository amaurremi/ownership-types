module State where

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> let (a, s') = runState m s
                             in runState (k a) s'

instance Applicative (State s) where
    pure x = error ""
    x <*> y = error ""

instance Functor (State s) where
    fmap _ _ = error ""


get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put newState = State $ \_ -> ((), newState)

