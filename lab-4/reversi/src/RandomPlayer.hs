module RandomPlayer where

import Reversi
import Grid
import System.Random
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NL

play :: Color -> Ply -> IO Grid
play c (g, gs) = (gs NL.!!) <$> randomRIO (0, length gs - 1)
