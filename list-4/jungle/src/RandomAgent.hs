module RandomAgent where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import           Data.Function
import           Data.List
import qualified Data.Map as Map

import System.Random

import Jungle

data Ctx = Ctx { total :: TVar Int }

turn :: Player -> (State, Int) -> IO (State, Int)
turn p s@(b, c) = do
  let ms = moves p s
      n = length ms
  if null ms
    then return (b, c + 1)
    else do
      i <- randomRIO (0, n - 1)
      return $ ms !! i

playRandom :: Ctx -> Player -> (State, Int) -> IO Player
playRandom ctx p s = case winner s of
  Just p -> return p
  Nothing -> do
    s' <- turn p s
    atomically $ modifyTVar' (total ctx) (+1)
    playRandom ctx (other p) s'

randomAgent :: Player -> (State, Int) -> IO (State, Int)
randomAgent p s@(b, c) = do
  scores <- atomically $ newTVar (Map.empty :: Map.Map (State, Int) Int)
  total <- atomically $ newTVar (0 :: Int)
  let ms = moves p s ++ ms
      ctx = Ctx total
      f x Nothing  = Just 0
      f x (Just v) = Just $ x + v
      aux (m:ms) = do
        c <- atomically $ readTVar total
        if c > 20000
          then return ()
          else do
            s <- playRandom ctx (other p) m
            atomically $ modifyTVar' scores (Map.alter (f s) m)
            aux ms
  if null ms
    then return (b, c + 1)
    else do
      aux ms
      res <- atomically $ readTVar scores
      let (m, _) = maximumBy (compare `on` snd) $ Map.toList res
      return m
