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
import Data.Foldable
import Data.Vector.Instances
import Data.Vector.Unboxed (Vector)

data Mode = Timed Int | Iter Int | Fixed Int

data Ctx = Ctx
  { mode :: Mode
  , doSort :: Bool
  , hParam :: Vector Double
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

-- score :: Tree -> Double
-- score (Node c s _ _) = fromIntegral (other c) * s

-- startTree :: Vector Double -> Color -> Grid -> Tree
-- startTree w c g = let s = eval w g in
--   Node c s g [startTree w (other c) g' | g' <- moves g c]

-- sortChildren :: [Tree] -> [Tree]
-- sortChildren = sortOn (\n -> -1 * score n)

negamax :: Ctx -> Int -> Color -> Double -> Double -> Tree -> IO Double
negamax ctx depth c alpha beta tree
  | isTerminal tree = do
      let s = finalScore $ grid tree
      -- modifyIORef' (leaves ctx) (+ 1)
      return s
  | depth == 0 = do
      -- modifyIORef' (heuristic ctx) (+ 1)
      return $ eval' (grid tree)
  | otherwise = do
      val <- newIORef negInf
      -- modifyIORef' (inner ctx) (+ 1)
      -- putStrLn $ "beginning work at depth: " ++ show depth
      let sorted = if c == white then whiteM tree else blackM tree
      let aux !_ !_ [] = readIORef val
          aux !alpha !beta (n:ns) = do
            n' <- (fromIntegral c *) <$> negamax ctx (depth - 1) (other c) (- beta) (- alpha) n
            modifyIORef' val (max n')
            alpha' <- max alpha <$> readIORef val
            if alpha' >= beta
              then readIORef val -- return (n' : ns)
              else aux alpha' beta ns
      -- aux alpha beta negInf gs
      -- children <- aux alpha beta sorted
      value <- aux alpha beta sorted
      -- let s = fromIntegral color * value
      -- putStrLn $ "depth: " ++ show depth ++ " value: " ++ show value
      return $ value -- Node color s grid children 

-- showTree :: Int -> Tree -> String
-- showTree 1 n@(Node c s g ts) = "( " ++ show (value n) ++ " " ++ show (map value ts) ++ " )"
-- showTree 2 n@(Node c s g ts) = "( " ++ show (value n) ++ "\n" ++ unlines (map (\t -> "  " ++ showTree 1 t) ts) ++ ")"

-- timed :: Ctx -> TVar Tree -> Int -> IO ()
-- timed ctx var k = do
--   tree <- atomically $ readTVar var
--   tree'@(Node !_ !s' !_ !ts) <- negamax ctx k negInf posInf tree
--   evaluate $ ts
--   atomically $ writeTVar var tree'
--   if score tree' == score tree 
--     then return ()
--     else timed ctx var (k + 1)

-- depth :: Ctx -> Int -> Tree -> IO Tree
-- depth ctx k tree = do
--   tree@(Node !_ !_ !_ !ts) <- negamax ctx k negInf posInf tree
--   evaluate ts
--   return tree

-- search :: Ctx -> Color -> Ply -> IO Grid
-- search ctx c (g, gs) =
--   let begin = startTree (hParam ctx) c g
--   in grid . head . sortChildren . children <$>
--     case mode ctx of
--       Timed t -> do
--         var <- atomically $ newTVar begin
--         thunk <- timeout t (timed ctx var 1)
--         evaluate thunk
--         atomically $ readTVar var
--       Iter k -> do
--         var <- atomically $ newTVar begin
--         for_ [1 .. k] (\i -> do
--           t <- atomically $ readTVar var
--           t' <- depth ctx i t
--           atomically $ writeTVar var t'
--           )
--         atomically $ readTVar var
--       Fixed k ->
--         depth ctx k begin
