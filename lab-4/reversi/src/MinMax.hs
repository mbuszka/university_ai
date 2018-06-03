{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}

module MinMax 
  ( fixed
  , timed
  ) where

import Reversi
import Engine

import Debug.Trace

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Exception.Base (evaluate)
import System.Timeout
import Data.List
import Data.Maybe
import Data.Function
import Data.Foldable
import Data.Vector.Instances
import qualified Data.Vector         as BVec
import qualified Data.Vector.Generic as Vec
import qualified Data.Vector.Unboxed as UVec

score :: Color -> Double -> Double
score col v = fromIntegral col * v

traceNM col depth val x =
  let msg = "color: " ++ (if col == white then "W" else "B") ++ " depth: " ++ show depth ++ " val: " ++ show val
  in trace msg x 

negamax :: Int -> Color -> Double -> Double -> Grid -> Double
negamax depth color alpha beta grid
  | Vec.null (moves color grid) && Vec.null (moves (other color) grid) =
    score color $ finalScore $ grid
  | depth == 0 =
    score color $ eval' grid
  | otherwise = let
      children = Vec.map (change color grid) $ moves color grid
      aux !alpha !beta !val cs =
        if Vec.null cs then val
        else let
          n' = negate $ negamax (depth - 1) (other color) (- beta) (- alpha) (BVec.head cs)
          val' = max val n'      
          alpha' = max alpha val'
        in if alpha' >= beta
          then val'
          else aux alpha' beta val' (BVec.tail cs)
    in aux alpha beta negInf children

best :: Color -> Grid -> Int -> Coord
best col grid depth = let
    children = moves col grid
    scores = Vec.map (\c -> (c, negamax depth (other col) negInf posInf (change col grid c))) children
  in fst $ Vec.minimumBy (compare `on` snd) scores

timed :: Player
timed t col grid = do
  var <- atomically $ newTVar (-1, -1)
  let
    us = floor $ t * 1000000
    loop k = do
      let val = best col grid k
      evaluate val
      atomically $ writeTVar var val
      loop $ k + 1
  thunk <- timeout us $ loop 0
  evaluate thunk
  atomically $ readTVar var

fixed :: Int -> Agent
fixed k col grid = return $ best col grid k