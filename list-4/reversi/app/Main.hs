module Main where

import Control.Monad
import Data.Maybe
import Data.IORef

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
      f <- score <$> oneGame g play play
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
  acc <- newIORef []
  let 
    f c p = do
      g <- play c p
      modifyIORef' acc (eval g :)
      return g
  -- replicateM_ 10000 (oneGame initial f f)
  -- l <- readIORef acc
  -- putStrLn $ "minimum: " ++ show (minimum l) ++ " max: " ++ show (maximum l)
  scores <- replicateM 1000 (score <$> oneGame initial searchM play)
  print $ result scores
  -- trains 1000 (Vec.replicate 64 0)
