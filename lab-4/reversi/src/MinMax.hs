{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}

module MinMax where

import Reversi
import Engine

import Debug.Trace

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Exception.Base (evaluate)
import System.Timeout
import Data.IORef
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Function
import Data.Foldable
import Data.Vector.Instances
import qualified Data.Vector         as BVec
import qualified Data.Vector.Generic as Vec
import qualified Data.Vector.Unboxed as UVec

data Mode = Timed Int | Fixed Int

data Ctx = Ctx
  { mode   :: Mode
  , doSort :: Bool
  , eval   :: Grid -> Double
  }

score :: Color -> Double -> Double
score col v = fromIntegral col * v

traceNM col depth val x =
  let msg = "color: " ++ (if col == white then "W" else "B") ++ " depth: " ++ show depth ++ " val: " ++ show val
  in trace msg x 

negamax :: Ctx -> Int -> Color -> Double -> Double -> Grid -> Double
negamax ctx depth color alpha beta grid
  | Vec.null (moves color grid) && Vec.null (moves (other color) grid) =
    score color $ finalScore $ grid
  | depth == 0 =
    score color $ eval ctx grid
  | otherwise = let
      children = Vec.map (change color grid) $ moves color grid
      aux !alpha !beta !val cs =
        if Vec.null cs then val
        else let
          n' = negate $ negamax ctx (depth - 1) (other color) (- beta) (- alpha) (BVec.head cs)
          val' = max val n'      
          alpha' = max alpha val'
        in if alpha' >= beta
          then val'
          else aux alpha' beta val' (BVec.tail cs)
    in aux alpha beta negInf children

best :: Ctx -> Color -> Grid -> Int -> Coord
best ctx col grid depth = let
    children = moves col grid
    scores = Vec.map (\c -> (c, negamax ctx depth (other col) negInf posInf (change col grid c))) children
  in fst $ Vec.minimumBy (compare `on` snd) scores


timed :: Ctx -> TVar Coord -> Color -> Grid -> Int -> IO ()
timed ctx res col tree k = do
  let val = best ctx col tree k
  evaluate val
  atomically $ writeTVar res val
  timed ctx res col tree (k + 1)

search :: Ctx -> Color -> Agent
search ctx col grid =
    case mode ctx of
      Timed t -> do
        var <- atomically $ newTVar (0, 0)
        thunk <- timeout t (timed ctx var col grid 0)
        evaluate thunk
        atomically $ readTVar var
      Fixed k ->
        return $ best ctx col grid k
