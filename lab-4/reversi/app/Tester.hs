module Tester where

import Control.Monad
import Data.Foldable

import Reversi
import Engine
import RandomPlayer
import MinMax
import MCTS

test :: Agent -> Agent -> IO ()
test p1 p2 = do
  let players = \c -> if c == white then p1 white else p2 black
  s1 <- replicateM 5 (game white players initial)
  s2 <- replicateM 5 (game black players initial)
  print $ fold $ map winner (s1 ++ s2)