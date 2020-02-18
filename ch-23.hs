import System.Random
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum { getSum :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

-- newtype State s a = State { runState :: s -> (a, s) }
-- State :: (s -> (a, s)) -> State s a
-- runState :: State s a -> s -> (a, s)

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1,6) s
      (d2, s2) = randomR (1,6) s1
      (d3, _)  = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in  go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1,6) gen
        in  go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 g []
  where
    go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
    go sum count gen log
      | sum >= n = (count, log)
      | otherwise =
        let (die, nextGen) = randomR (1,6) gen
            dieSide = intToDie die
        in  go (sum + die) (count + 1) nextGen (dieSide : log)

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  --fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ (\tuple -> (f (fst tuple), snd tuple)) . g

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) = Moi $ \s -> (fst (f s) ((fst . g) ((snd . f) s)),
                                     ((snd . g) ((snd .f) s)))

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $ \s -> (runMoi (g (fst (f s)))) s

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0  = "Buzz"
  | n `mod` 3 == 0  = "Fizz"
  | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main =
  mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]
