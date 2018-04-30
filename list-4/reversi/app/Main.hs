module Main where

import Control.Monad
import Data.Maybe

import Reversi
import RandomPlayer
import MinMax


main :: IO ()
main = do
  -- putStrLn $ showGrid initial
  -- let 
  --   moves = foldl (>=>) return $ replicate 5 (\b -> do
  --     b' <- play Black b
  --     putStrLn $ showGrid b'
  --     b'' <- pure $ search White b'
  --     putStrLn $ showGrid b''
  --     return b'')
  -- moves initial
  -- return ()
  scores <- replicateM 100 (playOne initial)
  print $ foldr (\s c -> if s > 0 then c + 1 else c) 0 scores

playOne :: Grid -> IO Double
playOne g = if end g
  then do
    -- putStrLn $ showGrid g
    return $ score White g
  else do
    w <- return $ search White g
    case w of
      Nothing -> do
        Just g' <- play Black g
        playOne g'
      Just g  -> do
        g' <- play Black g
        playOne $ fromMaybe g g'
