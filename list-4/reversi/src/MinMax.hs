module MinMax where

import Reversi
import Data.List
import Data.Function

limit = 3

maxVal :: Int -> Color -> Double -> Double -> Grid -> Double
maxVal depth c a b g = if depth >= limit || end g
  then eval c g + score c g
  else aux a b (-1.0 / 0) (moves g c)
  where 
    aux a b v [] = v
    aux a b v (s:ss) =
      let v' = max v (minVal (depth + 1) (other c) a b s)
      in if v' >= b then v'
      else aux (max a v') b v' ss

minVal :: Int -> Color -> Double -> Double -> Grid -> Double
minVal depth c a b g = if depth >= limit || end g
  then eval c g + score c g
  else aux a b (1.0 / 0) (moves g c)
  where
    aux a b v [] = v
    aux a b v (s:ss) =
      let v' = min v (maxVal (depth + 1) (other c) a b s)
      in if v' <= a then v'
      else aux a (min b v') v' ss

search :: Color -> Grid -> Maybe Grid
search c g =
  let m = (\s -> (s, maxVal 0 c (-1.0 / 0) (1.0 / 0) s)) <$> moves g c
  in if null m then Nothing else Just . fst . minimumBy (compare `on` snd) $ m
