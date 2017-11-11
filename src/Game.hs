module Game 
  ( game
  ) where

import Control.Monad (void)
import Control.Concurrent
import Control.Concurrent.MVar
import Tamagotchi

-- observation:
--  the mechanism of concurrency must exist before events can be applied to it.
--    seems pretty obvious once you say it out loud.
--    examples of the mechanism:
--      * MVar, or some other form of lock like a semaphore
--      * a queue to pass events around

microSecsPerTick :: Int
microSecsPerTick = 1000000

game :: Tamagotchi -> IO ()
game tama = do
  tamaMVar <- newMVar tama
  userActionThread <- forkIO $ performUserAction tamaMVar
  gameLoop tamaMVar userActionThread

gameLoop :: MVar Tamagotchi -> ThreadId -> IO ()
gameLoop tamaMVar userActionThread = do
  curTama <- updateTama tamaMVar defaultTick
  let isGameOver = isDead curTama
  if isGameOver
    then do
      killThread userActionThread
      putStrLn "Game Over"
    else do
      printCurrentState curTama
      threadDelay microSecsPerTick
      gameLoop tamaMVar userActionThread

updateTama :: MVar Tamagotchi ->
  (Tamagotchi -> Tamagotchi) ->
  IO Tamagotchi
updateTama mvar action = do
  tama <- takeMVar mvar
  let newTama = action tama
  putMVar mvar newTama
  pure newTama

performUserAction :: MVar Tamagotchi -> IO ()
performUserAction tamaMVar = do
  inputChar <- getChar
  case inputChar of
    'h' -> printUserOptions
    'f' -> void $ updateTama tamaMVar defaultFeed
    _ -> pure ()
  performUserAction tamaMVar

printCurrentState :: Tamagotchi -> IO ()
printCurrentState tama = putStrLn $ 
  "== " ++ name tama ++ 
  " == Age: " ++ show (age tama) ++
  " == Hunger: " ++ show (hunger tama) ++
  " =="

printUserOptions :: IO ()
printUserOptions = putStrLn 
  "** Input Options: 'f' - feed; 'h' - help **"