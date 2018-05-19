module RandomPlayer where

import Reversi
import Grid
import System.Random
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NL

play :: Color -> Tree -> IO Tree
play c (Tree g wm bm) = 
  let gs = if c == white then wm else bm
  in (gs !!) <$> randomRIO (0, length gs - 1)
