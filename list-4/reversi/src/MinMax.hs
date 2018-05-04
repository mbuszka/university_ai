{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns, DeriveGeneric, DeriveAnyClass #-}

module MinMax where

import Reversi
import Grid

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception.Base (evaluate)
import System.Timeout
import Data.IORef
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.List.NonEmpty as NL
import qualified Data.HashTable.IO as H
import qualified Data.HashTable.IO as BH
import Data.Vector.Instances

type HashTable k v = BH.BasicHashTable k v

data Ctx = Ctx
  { leaves :: IORef Int
  , heuristic :: IORef Int
  , inner :: IORef Int
  }

showCtx :: Ctx -> IO ()
showCtx ctx = do
  l <- readIORef $ leaves ctx
  h <- readIORef $ heuristic ctx
  i <- readIORef $ inner ctx
  putStrLn $ "inner: " ++ show i ++ " leaves: " ++ show l ++ " heuristic: " ++ show h 

data Tree = Node 
  { color :: Color
  , value :: Double
  , grid :: Grid
  , children :: [Tree]
  }
  deriving (Generic, NFData)

score :: Tree -> Double
score (Node c s _ _) = fromIntegral (other c) * s

startTree :: Color -> Grid -> Tree
startTree c g = let s = eval g in
  Node c s g [startTree (other c) g' | g' <- moves g c]

sortChildren :: [Tree] -> [Tree]
sortChildren = sortOn (\n -> -1 * score n)

newCtx :: () -> IO Ctx
newCtx () = Ctx <$> newIORef 0 <*> newIORef 0 <*> newIORef 0

negamax :: Ctx -> Int -> Double -> Double -> Tree -> IO Tree
negamax ctx depth alpha beta tree@(Node color _ grid children)
  | end grid = do
      let s = finalScore grid
      modifyIORef' (leaves ctx) (+ 1)
      return $ Node color s grid children
  | depth == 0 = do
      modifyIORef' (heuristic ctx) (+ 1)
      return tree
  | otherwise = do
      val <- newIORef negInf
      modifyIORef' (inner ctx) (+ 1)
      -- putStrLn $ "beginning work at depth: " ++ show depth
      let sorted = sortChildren children
      let aux !_ !_ [] = return []
          aux !alpha !beta (n:ns) = do
            n' <- negamax ctx (depth - 1) (- beta) (- alpha) n
            modifyIORef' val (max (score n'))
            alpha' <- max alpha <$> readIORef val
            if alpha' >= beta
              then return (n' : ns)
              else (n' :) <$> aux alpha' beta ns
      -- aux alpha beta negInf gs
      children <- aux alpha beta sorted
      value <- readIORef val
      let s = fromIntegral color * value
      -- putStrLn $ "depth: " ++ show depth ++ " value: " ++ show value
      return $ Node color s grid children 

showTree :: Int -> Tree -> String
showTree 1 n@(Node c s g ts) = "( " ++ show (value n) ++ " " ++ show (map value ts) ++ " )"
showTree 2 n@(Node c s g ts) = "( " ++ show (value n) ++ "\n" ++ unlines (map (\t -> "  " ++ showTree 1 t) ts) ++ ")"

  
-- maxVal :: Int -> Color -> Double -> Double -> Grid -> Double
-- maxVal depth c a b g = if end g
--   then score c g
--   else if depth >= limit
--   then eval c g
--   else aux a b (-1.0 / 0) (ordered c $ moves g c)
--   where 
--     aux a b v [] = v
--     aux a b v (s:ss) =
--       let v' = max v (minVal (depth + 1) (other c) a b s)
--       in if v' >= b then v'
--       else aux (max a v') b v' ss

-- minVal :: Int -> Color -> Double -> Double -> Grid -> Double
-- minVal depth c a b g = if end g
--   then score c g
--   else if depth >= limit
--   then eval c g
--   else aux a b (1.0 / 0) (ordered c $ moves g c)
--   where
--     aux a b v [] = v
--     aux a b v (s:ss) =
--       let v' = min v (maxVal (depth + 1) (other c) a b s)
--       in if v' <= a then v'
--       else aux a (min b v') v' ss

ordered c = id -- sortBy (revcompare `on` score c)

revcompare a b = case compare a b of
  GT -> LT
  EQ -> EQ
  LT -> GT

mySeq [] = []
mySeq ((!h):t) = h:mySeq t 

forceTree :: Tree -> Tree
forceTree (Node !c !s !g ts) = Node c s g (mySeq ts) 

work :: Ctx -> TVar Tree -> Int -> IO ()
work ctx var k = do
  -- ctx <- newCtx ()
  -- putStrLn $ "iteration: " ++ show k
  tree <- atomically $ readTVar var
  tree'@(Node !_ !s' !_ !ts) <- negamax ctx k negInf posInf tree
  evaluate $ ts
  atomically $ writeTVar var tree'
  if score tree' == score tree 
    then return ()
    else work ctx var (k + 1)


search :: Int -> Color -> Ply -> IO Grid
search time c (g, gs) =
  let tree = startTree c g
  in do
    ctx <- newCtx ()
    var <- atomically $ newTVar (startTree c g)
    -- putStrLn "forking"
    -- id <- forkIO (work ctx var 1)
    v <- timeout time (work ctx var 1)
    evaluate v
    -- putStrLn "woke up"
    -- killThread id
    -- putStrLn "worker killed"
    -- work ctx var 1
    -- work ctx var 2
    -- work ctx var 3
    -- work ctx var 4
    -- showCtx ctx
    -- work var 
    tree <- atomically $ readTVar var
    -- putStrLn $ showTree 2 tree
    return $ grid . head . sortChildren . children $ tree
