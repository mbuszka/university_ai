module Main where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.List
import Data.Function

import System.Random
import System.Environment
import Reversi
import Grid
import RandomPlayer
import MinMax
import qualified Data.Vector.Unboxed as Vec
import           Data.Vector.Unboxed (Vector)

randomVsRandom :: Grid -> IO Grid
randomVsRandom g = oneGame g play play

oneTurn :: Grid -> (Color -> Ply -> IO Grid) -> (Color -> Ply -> IO Grid) -> IO (Maybe Grid)
oneTurn g w b = case ply g white of
  Just p -> do
    g' <- w white p
    case ply g' black of
      Just p -> Just <$> b black p
      Nothing -> return $ Just g'
  Nothing -> case ply g black of
    Just p -> Just <$> b black p
    Nothing -> return Nothing

kTurns :: Int -> Grid -> (Color -> Ply -> IO Grid) -> (Color -> Ply -> IO Grid) -> IO (Maybe Grid)
kTurns 0 g _ _ = return $ Just g
kTurns k g w b = do
  g' <- oneTurn g w b
  case g' of
    Nothing -> return Nothing
    Just g -> kTurns (k-1) g w b
    
train :: Int -> Vector Double -> IO (Vector Double)
train k weights = do
  g <- kTurns k initial play play
  case g of
    Nothing -> return weights
    Just g -> do
      f <- evalPosession <$> oneGame g play play
      return $ Vec.zipWith (+) weights . Vec.map (\i -> fromIntegral i * (f * 0.01)) $ g

oneGame :: Grid -> (Color -> Ply -> IO Grid) -> (Color -> Ply -> IO Grid) -> IO Grid
oneGame init w b = loop init
  where
    loop g = do
      g' <- oneTurn g w b
      case g' of
        Nothing -> return g
        Just g -> loop g
      
result :: [Double] -> Int
result = length . filter (>0)

trains :: Int -> Vector Double -> IO ()
trains 0 weights = print weights
trains k weights = train 10 weights >>= trains (k-1)

main :: IO ()
main = do
  comStr:modStr:numStr:t <- getArgs
  let n = read numStr
  let 
    mode = case modStr of
      "iter" -> Iter n
      "time" -> Timed n
      "fixed" -> Fixed n
    doSort = null t
    ctx = Ctx mode doSort (Vec.fromListN 3 [5.56, 9.74, 9.68])
  -- acc <- newIORef []
  -- let 
  --   f c p = do
  --     g <- play c p
  --     modifyIORef' acc (eval g :)
  --     return g
  -- replicateM_ 10000 (oneGame initial f f)
  -- l <- readIORef acc
  -- putStrLn $ "minimum: " ++ show (minimum l) ++ " max: " ++ show (maximum l)
  case comStr of
    "run" -> do
      scores <- replicateM 1000 (evalPosession <$> oneGame initial (search ctx) play)
      print $ result scores
    "train" -> let 
        one = do
          weights <- Vec.fromListN 3 <$> replicateM 3 (randomRIO (1, 1000))
          let ctx' = ctx { hParam = weights }
          score <- result <$> replicateM 1000 (evalPosession <$> oneGame initial (search ctx') play)
          print (weights, score)
          return (weights, score)
      in do
        scores <- replicateM 100 one
        let top = maximumBy (compare `on` snd) scores
        putStrLn $ "best: " ++ show top
        -- print top

      
  -- trains 1000 (Vec.replicate 64 0)
