module Tester where

import Control.Monad

import Reversi
import Engine
import RandomPlayer
import MinMax
import MCTS
      
result :: [Double] -> Int
result = length . filter (>0)

bench :: IO ()
bench = do
  scores <- replicateM 1000 (evalPosession <$> game white play initial)
  print $ result scores

test :: Ctx -> IO ()
test ctx = do
  let players = \c -> if c == white then mctsAgent 0.5 white else search ctx black
  scores <- replicateM 5 (evalPosession <$> game white players initial)
  scores2 <- replicateM 5 (evalPosession <$> game black players initial)
  print $ result scores
  print $ result scores2