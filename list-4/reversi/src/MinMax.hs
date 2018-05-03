{-# LANGUAGE NamedFieldPuns #-}
module MinMax where

import Reversi
import Grid
import Data.IORef
import Data.List
import Data.Function
import qualified Data.List.NonEmpty as NL
import qualified Data.HashTable.IO as H
import qualified Data.HashTable.IO as BH
import Data.Vector.Instances

type HashTable k v = BH.BasicHashTable k v

posInf :: Double
posInf = 1 / 0

negInf :: Double
negInf = - posInf

data Ctx = Ctx
  { cache :: HashTable Grid Double
  , succesfull :: IORef Int
  , total :: IORef Int
  }

data Tree = Node Color Grid [Tree]

startTree :: Color -> Grid -> Tree
startTree c g = Node c g [startTree (other c) g' | g' <- moves g c]

sortChildren :: Ctx -> Tree -> IO Tree
sortChildren ctx (Node c g gs) = do
  gs' <- traverse (\(Node c g gs) -> do
    let f Nothing = let s = eval g in (Just s, s)
        f (Just s) = (Just s, s)
    s <- H.mutate (cache ctx) g f
    return (fromIntegral c * s, Node c g gs)) gs
  let sorted = map snd $ sortOn fst gs'
  return $ Node c g sorted

newCtx :: () -> IO Ctx
newCtx () = do
  cache <- H.new
  succesfull <- newIORef 0
  total <- newIORef 0
  return $ Ctx { cache, succesfull, total }

negamax :: Ctx -> Int -> Double -> Double -> Color -> Tree -> IO Double
negamax ctx depth alpha beta color tree@(Node _ grid _)
  | end grid = do
      let s = score grid
      H.insert (cache ctx) grid s
      return (fromIntegral color * s)
  | depth == 0 = do
    let s = eval grid
    H.insert (cache ctx) grid s
    return (fromIntegral color * s)
  | otherwise = do
      -- print "sorting"
      Node _ _ gs <- return tree -- sortChildren ctx tree
      -- print "sorted"
      let aux _ _ val [] = return val
          aux alpha beta val (n:ns) = do
            let Node _ g _ = n
            -- mv <- H.lookup (cache ctx) g
            -- modifyIORef' (total ctx) (+ 1)
            -- val' <- case mv of
            --   Just v -> do
            --     modifyIORef' (succesfull ctx) (+ 1) 
            --     return v
            --   Nothing -> do
            val' <- negate <$> negamax ctx (depth - 1) (- beta) (- alpha) (other color) n
            H.insert (cache ctx) g val'
            -- return v
            let best = max val val'
                alpha' = max alpha val'
            if alpha >= beta
              then return $ best
              else aux alpha' beta best ns
      aux alpha beta negInf gs

  
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

search :: Color -> Ply -> IO Grid
search c (g, gs) =
  let tree = startTree c g
  in do
    ctx <- newCtx ()
    -- print "begin"
    negamax ctx 1 negInf posInf c tree
    -- print "done lvl 1"
    negamax ctx 2 negInf posInf c tree
    -- print "done lvl 2"
    -- negamax ctx 3 negInf posInf c tree
    -- print "done lvl 3"
    Node _ _ gs <- sortChildren ctx tree
    -- m <- mapM (\s -> (,) s <$> negamax ctx 2 negInf posInf c s) ms
    -- s <- readIORef (succesfull ctx)
    -- t <- readIORef (total ctx)
    -- print $ "total lookups: " ++ show t
    -- print $ "succesfull percentage: " ++ show (100 * fromIntegral s / fromIntegral t)
    return $ (\(Node _ g _) -> g) $ head gs

searchM :: Color -> Ply -> IO Grid
searchM c p = search c p
