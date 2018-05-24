module RandomPlayer where

import Reversi
import Engine
import System.Random
import qualified Data.Vector.Generic as Vec

play :: Color -> Agent
play color grid = 
  let gs = moves color grid
  in (gs Vec.!) <$> randomRIO (0, Vec.length gs - 1)
