module Tester where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.List
import Data.Function

import System.Random
import Reversi
import Grid
import RandomPlayer
import MinMax
import qualified Data.Vector.Unboxed as Vec
import           Data.Vector.Unboxed (Vector)

-- randomVsRandom :: Grid -> IO Grid
-- randomVsRandom g = oneGame g (play white) (play black)

oneTurn :: (Tree -> IO Tree) -> (Tree -> IO Tree) -> Tree -> IO Tree
oneTurn w b t = do
  t' <- if hasMoves white t
    then w t else return t
  if hasMoves black t' then b t' else return t'

-- kTurns :: Int -> Grid -> (Color -> Ply -> IO Grid) -> (Color -> Ply -> IO Grid) -> IO (Maybe Grid)
-- kTurns 0 g _ _ = return $ Just g
-- kTurns k g w b = do
--   g' <- oneTurn g w b
--   case g' of
--     Nothing -> return Nothing
--     Just g -> kTurns (k-1) g w b
    
-- train :: Int -> Vector Double -> IO (Vector Double)
-- train k weights = do
--   g <- kTurns k initial play play
--   case g of
--     Nothing -> return weights
--     Just g -> do
--       f <- evalPosession <$> oneGame g play play
--       return $ Vec.zipWith (+) weights . Vec.map (\i -> fromIntegral i * (f * 0.01)) $ g

oneGame :: Grid -> (Tree -> IO Tree) -> (Tree -> IO Tree) -> IO Grid
oneGame init w b = loop (gameTree init)
  where
    loop t = if isTerminal t
      then return $ grid t
      else oneTurn w b t >>= loop
      
result :: [Double] -> Int
result = length . filter (>0)

-- trains :: Int -> Vector Double -> IO ()
-- trains 0 weights = print weights
-- trains k weights = train 10 weights >>= trains (k-1)

test ctx = do
  scores <- replicateM 1000 (evalPosession <$> oneGame initial (play white) (play black))
  print $ result scores

-- runTraining ctx = 
--   let
--     one = do
--       weights <- Vec.fromListN 3 <$> replicateM 3 (randomRIO (1, 1000))
--       let ctx' = ctx { hParam = weights }
--       score <- result <$> replicateM 1000 (evalPosession <$> oneGame initial (search ctx') play)
--       print (weights, score)
--       return (weights, score)
--   in do
--     scores <- replicateM 200 one
--     let top = maximumBy (compare `on` snd) scores
--     putStrLn $ "best: " ++ show top
--         -- print top
