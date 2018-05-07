module Main where

import Control.Monad
import System.Environment

import Jungle
import RandomAgent
import HeuristicAgent

play :: (Player -> (State, Int) -> IO (State, Int))
     -> Player
     -> (State, Int)
     -> IO Player
play f p s = case winner s of
  Just p -> return p
  Nothing -> do
    s' <- f p s
    play f (other p) s'

hVr :: Player -> (State, Int) -> IO (State, Int)
hVr 1 = heuristicAgent 1
hVr (-1) = randomAgent (-1)

rVr 1 = randomAgent 1
rVr (-1) = turn (-1)

main :: IO ()
main = do
  arg:_ <- getArgs
  let f = case arg of
        "h" -> hVr
        "r" -> rVr
  scores <- replicateM 10 (play f 1 (initial, 0))
  print scores
