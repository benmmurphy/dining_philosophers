module Dining where
import Control.Concurrent
import System.Random
import Control.Monad
import Control.Concurrent.STM

type Fork = TMVar ()

type Philosopher = IO ()

names :: [String]
names = ["Plato", "Nietzche", "Kant", "Socrates", "Aristotle", "Hume"]


microToSecond :: Int -> Int
microToSecond micro =  1000 * 1000 * micro

wait :: IO ()
wait = do 
       time <- randomRIO (microToSecond 1 , microToSecond 3)
       threadDelay (time)

philosopher :: Chan String -> (String, Fork, Fork) -> Philosopher
philosopher queue (name, left, right) = forever $ do
  writeChan queue $ name ++ " waiting..."
  wait
  writeChan queue $ name ++ " is hungry.."
  eaten <- atomically $ (do
    takeTMVar left
    takeTMVar right
    return True) `orElse` return False

  if eaten then do
    writeChan queue $ name ++ " eating..."
    atomically $ putTMVar left ()
    atomically $ putTMVar right ()
  else
    writeChan queue $ name ++ " did not eat.."
  yield

logger :: Chan String -> IO ()
logger messageQueue = forever $ do
   putStrLn =<< readChan messageQueue

main :: IO ()
main = do 
   messageQueue <- newChan :: IO (Chan String)
   let maker = philosopher messageQueue
   forks <- atomically $ mapM (const $ newTMVar ()) names
   let places = zip3 names forks (tail $ cycle forks)
   mapM_ (forkOS . maker) places
   logger messageQueue
