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

data Mode = Timed Int | Iter Int | Fixed Int

data Ctx = Ctx
  { mode   :: Mode
  , doSort :: Bool
  , eval   :: Grid -> Double
  }

-- showCtx :: Ctx -> IO ()
-- showCtx ctx = do
--   l <- readIORef $ leaves ctx
--   h <- readIORef $ heuristic ctx
--   i <- readIORef $ inner ctx
--   putStrLn $ "inner: " ++ show i ++ " leaves: " ++ show l ++ " heuristic: " ++ show h 

-- data Tree = Node 
--   { color :: Color
--   , value :: Double
--   , grid :: Grid
--   , children :: [Tree]
--   }
--   deriving (Generic, NFData)

score :: Color -> Double -> Double
score col v = fromIntegral col * v

-- startTree :: Vector Double -> Color -> Grid -> Tree
-- startTree w c g = let s = eval w g in
--   Node c s g [startTree w (other c) g' | g' <- moves g c]

-- sortChildren :: [Tree] -> [Tree]
-- sortChildren = sortOn (\n -> -1 * score n)

traceNM col depth val x =
  let msg = "color: " ++ (if col == white then "W" else "B") ++ " depth: " ++ show depth ++ " val: " ++ show val
  in trace msg x 

negamax :: Ctx -> Int -> Color -> Double -> Double -> Tree -> Double
negamax ctx depth c alpha beta tree
  | isTerminal tree =
    score c $ finalScore $ grid tree
  | depth == 0 =
    score c $ eval ctx (grid tree)
  | otherwise = let
      children = fmap snd $ if c == white then whiteM tree else blackM tree
      aux !alpha !beta !val cs =
        if Vec.null cs then val
        else let
          n' = negate $ negamax ctx (depth - 1) (other c) (- beta) (- alpha) (BVec.head cs)
          val' = max val n'      
          alpha' = max alpha val'
        in if alpha' >= beta
          then val'
          else aux alpha' beta val' (BVec.tail cs)
    in aux alpha beta negInf children

best :: Ctx -> Color -> Tree -> Int -> (Coord, Tree)
best ctx col tree depth = let
    children = if col == white then whiteM tree else blackM tree
    scores = Vec.map (\(c, t) -> ((c, t), negamax ctx depth (other col) negInf posInf t)) children
    msg = show $ fmap (\((c, t), s) -> (c, s)) scores
  in fst $ Vec.minimumBy (compare `on` snd) scores


timed :: Ctx -> TVar (Coord, Tree) -> Color -> Tree -> Int -> IO ()
timed ctx res col tree k = do
  let val = best ctx col tree k
  evaluate val
  atomically $ writeTVar res val
  timed ctx res col tree (k + 1)

search :: Ctx -> Color -> Tree -> IO (Coord, Tree)
search ctx col tree =
    case mode ctx of
      Timed t -> do
        var <- atomically $ newTVar ((0, 0), tree)
        thunk <- timeout t (timed ctx var col tree 0)
        evaluate thunk
        atomically $ readTVar var
      -- Iter k -> do
      --   var <- atomically $ newTVar begin
      --   for_ [1 .. k] (\i -> do
      --     t <- atomically $ readTVar var
      --     t' <- depth ctx i t
      --     atomically $ writeTVar var t'
      --     )
      --   atomically $ readTVar var
      Fixed k ->
        return $ best ctx col tree k
