import Control.Monad (join, liftM2, liftM3, (>=>))
import Control.Applicative ((*>))

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

-- what Monad is not:
-- impure. monadic functions are pure functions
-- an embedded language for imperative programming.
-- a value.
-- about strictness. the monadic operations of bind and return are non-strict

-- the monad typeclass is generalized structure manipulation with some laws
-- to make it sensible.

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn ("why hello there " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "hello friend." >>
  putStrLn "name pls: " >>
  getLine >>= (\name -> putStrLn ("why hello there " ++ name))

-- Monad laws:
-- IDENTITY:
--   right identity
--     m >>= return == m
--   left identity
--     return x >>= f == f x
--   ****return should be neutral and not perform any computation
-- ASSOCIATIVITY:
--   regrouping the functions should not have any impact on the final result
--     (m >>= f) >>= g == m >>= (\x -> f x >>= g)

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))
-- using join and fmap(<$>) together means we can use >>=

mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = g a >>= f

sayHi :: String -> IO String
sayHi g = do
  putStrLn g
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello, how old are you? "
