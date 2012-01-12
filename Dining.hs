module Dining where
import Control.Concurrent.MVar
import Control.Concurrent
import System.Random
import Control.Monad

type Fork = MVar ()

type Philosopher = IO ()

names :: [String]
names = ["Plato", "Nietzche", "Kant", "Socrates", "Aristotle", "Hume"]


wait :: IO ()
wait = do 
       time <- randomRIO (1  * 1000 ,3  * 1000)
       threadDelay (time)

philosopher :: Chan String -> (String, Fork, Fork) -> Philosopher
philosopher queue (name, left, right) = forever $ do
  takeMVar left
  writeChan queue $ name ++ " picked up left fork, "
  takeMVar right
  writeChan queue $ name ++ " picked up right fork, "
  writeChan queue $ name ++ " eating"
  wait
  putMVar left ()
  writeChan queue $ name ++ " dropped left fork, "
  putMVar right ()
  writeChan queue $ name ++ " dropped right fork"

logger :: Chan String -> IO ()
logger messageQueue = forever $ do
   putStrLn =<< readChan messageQueue

main :: IO ()
main = do 
   messageQueue <- newChan :: IO (Chan String)
   let maker = philosopher messageQueue
   forks <- mapM (const $ newMVar ()) names
   let places = zip3 names forks (tail $ cycle forks)
   mapM_ (forkOS . maker) places
   logger messageQueue
