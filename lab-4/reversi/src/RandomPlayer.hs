module RandomPlayer where

import Reversi
import Engine
import System.Random
import qualified Data.Vector.Generic as Vec

play :: Color -> Tree -> IO Tree
play c (Tree g wm bm) = 
  let gs = if c == white then wm else bm
  in snd <$> (gs Vec.!) <$> randomRIO (0, Vec.length gs - 1)
