module RandomPlayer where

import Reversi
import System.Random
import qualified Data.Map as Map


play :: Color -> Grid -> IO (Maybe Grid)
play c g = let
  m = moves g c
  n = length m
  in if n == 0
    then return Nothing
    else Just . (m !!) <$> randomRIO (0, n-1)
