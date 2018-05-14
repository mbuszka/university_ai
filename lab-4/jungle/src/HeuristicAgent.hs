module HeuristicAgent where

import qualified Data.Map as Map
import Data.Ord
import Data.List

import Jungle

cave :: Player -> Coord
cave 1 = (0, 3)
cave (-1) = (7, 3)

h :: Player -> (State, Int) -> Int
h p s@(b, c) =
  let my = Map.filter ((== p) . fst) b
      val = sum . map (fromEnum . snd) . Map.elems $ my 
      dists = 10 : (map (dist (cave (other p))) . Map.keys $ my)
  in if winner s == Just p
    then maxBound
    else val + (10 - minimum dists) * 4
  

heuristicAgent :: Player -> (State, Int) -> IO (State, Int)
heuristicAgent p s@(b, c) = do
  -- putStrLn $ showS b
  let ms = moves p s
  if null ms
    then return (b, c + 1)
    else do
      let top = sortOn (Down . h p) ms
      return $ head top
