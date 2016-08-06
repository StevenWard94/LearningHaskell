-- vim: set tw=80 fo-=o fo-=t sw=4 sts=2 ts=8 et fdm=marker fmr=\\begin,\\end fdl=1:
-- |
-- Module:        Monads.Instances
-- Author:        Steven Ward <stevenward94@gmail.com>
-- URL:           https://github.com/StevenWard94/LearningHaskell.d
-- Last Change:   2016 Aug 06
--
module Monads.Instances where

-- The instances that used to be contained in Control.Monad.Instances were moved
-- to GHC.Base and Data.Either.
-- Control.Monad.Instances is now deprecated and contains no instances.
-- import Control.Monad.Instances        -- DEPRECATED
import System.Random
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.List as DL

--
-- I. The '(->) r' Monad Instance \begin1
--

-- instance Monad ((->) r) where
--     return x = \_ -> x
--     h >>= f = \w -> f (h w) w

addStuff :: Integer -> Integer
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)

addStuff' :: Integer -> Integer
addStuff' x = let
    a = (*2) x
    b = (+10) x
    in a + b


-- \end1
-- II. The 'State' Monad \begin1
--

-- The State instance of Monad was created because, as a pure language, haskell
-- can become quite tedious when modeling problems that are inherently "state-ful".

-- threeCoins function that uses the StdGen instance of RandomGen defined in System.Random
-- note: originally defined and explained in lesson from...
--         http://learnyouahaskell.com/input-and-output#randomness
threeCoins :: StdGen -> (Bool,Bool,Bool)
threeCoins gen =
    let (fstCoin,newGen)  = random gen
        (sndCoin,newGen') = random newGen
        (thdCoin,_)       = random newGen'
     in (fstCoin,sndCoin,thdCoin)
-- because Haskell is pure, the original 'gen' cannot be modified and thus we
-- must continually reproduce 'newGen's if we wish to generate multiple random
-- numbers

-- The 'State' monad allows Haskell to model "stateful computations" efficiently
-- without giving up any of its purity.
-- A "stateful computation" is essentialy a function that takes some state and
-- returns a value along with some new state. This sort of function would have
-- the type: s -> (a,s), where s is the type of the state and a is the result of
-- the stateful computations.


-- \end1
-- III. Stacks and Stones \begin1
--

type Stack = [Integer]

pop :: Stack -> (Integer,Stack)
pop (x:xs) = (x,xs)

push :: Integer -> Stack -> ((),Stack)
push a xs = ((),a:xs)

manip :: Stack -> (Integer,Stack)
manip stack = let ((),newStack1) = push 3 stack
                  (a,newStack2)  = pop newStack1
               in pop newStack2

stackManip :: Stack -> (Integer,Stack)
stackManip = do
    push 3
    a <- pop
    pop


-- \end1
-- IV. The State Monad \begin1
--
-- newtype State s a = State { runState :: s -> (a,s) }

-- instance Monad (State s) where
--     return x = State $ \s -> (x,s)
--     (State h) >>= f = State $ \s -> let (a,newState) = h s
--                                         (State g) = f a
--                                      in g newState
pop' :: State Stack Integer
pop' = state $ \(x:xs) -> (x,xs)

push' :: Integer -> State Stack ()
push' a = state $ \xs -> ((),a:xs)

stackManip' :: State Stack Integer
stackManip' = do
    push' 3
    pop'
    pop'

stackStuff :: State Stack ()
stackStuff = do
    a <- pop'
    if a == 5
       then push' 5
       else do
           push' 3
           push' 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip'
    if a == 100
       then stackStuff
       else return ()

myGet :: (MonadState s m) => m s
myGet = state $ \s -> (s,s)

myPut :: (MonadState s m) => s -> m ()
myPut newState = state $ \s -> ((),newState)

-- this function uses functions defined in the MonadState type class
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
       then put [8,3,1]
       else put [9,2,1]


-- \end1
-- V. Randomness and the State Monad \begin1
--

-- random from System.Random has the following type:
--     random :: (RandomGen g, Random a) => g -> (a,g)
-- this is also a stateful computation, so...
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

-- now back to the threeCoins function...
threeCoins' :: State StdGen (Bool,Bool,Bool)
threeCoins' = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)


-- \end1
-- VI. Some Useful Monad Functions \begin1
--

-- liftM
liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = do
    x <- m
    return (f x)

-- ap: basically (<*>) but with a Monad constraint instead of Applicative
ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf m = do
    f <- mf
    x <- m
    return (f x)

-- liftM2
liftM2' :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2' f m1 m2 = do
    x1 <- m1
    x2 <- m2
    return (f x1 x2)

-- join
join' :: (Monad m) => m (m a) -> m a
join' mm = do { m <- mm; m }

joinedMaybes :: Integer -> Maybe Integer
joinedMaybes x = do
    m <- Just $ Just x
    m

-- filterM
filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' _ []     = return []
filterM' p (x:xs) = do
    flg <- p x
    ys <- filterM' p xs
    return $ if flg then x:ys else ys

keepSmall :: Integer -> Writer [String] Bool
keepSmall x
  | x < 4     = do
      tell ["Keeping " ++ show x]
      return True
  | otherwise = do
      tell [show x ++ " is too large. Throwing it away."]
      return False

smallerList :: [Integer] -> [Integer]
smallerList xs = fst $ runWriter $ filterM keepSmall xs

keepSmallLog :: [Integer] -> IO ()
keepSmallLog xs = mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall xs

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True,False]) xs

-- foldM
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ a []      = return a
foldM' f a (x:xs)  = f a x >>= \fax -> foldM' f fax xs

binSmalls :: Integer -> Integer -> Maybe Integer
binSmalls acc x
  | x > 9     = Nothing
  | otherwise = Just (acc + x)

foldBinSmalls :: Integer -> [Integer] -> Maybe Integer
foldBinSmalls acc xs = foldM binSmalls acc xs


-- \end1
-- VII. Making a Safe RPN Calculator
--

-- previously, when examining this problem, the solveRPN function worked as long
-- as the input made sense but if something went wrong, the whole program would
-- crash
oldSolveRPN :: String -> Double
oldSolveRPN = head . foldl oldFoldFunc [] . words

oldFoldFunc :: [Double] -> String -> [Double]
oldFoldFunc (x:y:ys) "*" = (x * y):ys
oldFoldFunc (x:y:ys) "+" = (x + y):ys
oldFoldFunc (x:y:ys) "-" = (x - y):ys
oldFoldFunc xs numStr = read numStr:xs

-- Now, we can reimplement it using the Maybe Monad so that it can handle
-- erroneous input without crashing
readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of [(x,"")] -> Just x
                                  _        -> Nothing

foldFunc :: [Double] -> String -> Maybe [Double]
foldFunc (x:y:ys) "*" = return $ (x * y):ys
foldFunc (x:y:ys) "+" = return $ (x + y):ys
foldFunc (x:y:ys) "-" = return $ (x - y):ys
foldFunc xs numStr = liftM (:xs) $ readMaybe numStr

solveRPN :: String -> Maybe Double
solveRPN str = do
    [result] <- foldM foldFunc [] $ words str
    return result
