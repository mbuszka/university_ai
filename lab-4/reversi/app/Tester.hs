module Tester where

import Control.Monad

import Reversi
import Engine
import RandomPlayer
import MinMax
      
result :: [Double] -> Int
result = length . filter (>0)

bench :: IO ()
bench = do
  scores <- replicateM 1000 (evalPosession <$> oneGame initial (play white) (play black))
  print $ result scores

test :: Ctx -> IO ()
test ctx = do
  scores <- replicateM 1000 (evalPosession <$> oneGame initial (fmap snd . search ctx white) (play black))
  print $ result scores