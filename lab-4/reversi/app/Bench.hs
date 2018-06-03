module Bench 
  ( bench
  ) where

import Control.Monad
import Data.Foldable

import Engine
import RandomPlayer
import Reversi

bench :: IO ()
bench = do
  scores <- replicateM 1000 (game white play initial)
  print $ fold (map winner scores)