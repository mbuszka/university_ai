{-# LANGUAGE OverloadedStrings #-}

module Player where

import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Attoparsec.Text
import System.Exit
import System.IO

import Reversi
import Engine
import MinMax
import MCTS

data Command = HeDid Double Double Coord | UGo Double Double | OneMore | Bye
  deriving (Show)

parseCommand :: Parser Command
parseCommand = 
      (HeDid <$> ("HEDID" *> skipSpace *> double) <*> (skipSpace *> double) <*> ((,) <$> (skipSpace *> signed decimal) <*> (skipSpace *> signed decimal)))
  <|> (UGo <$> ("UGO" *> skipSpace *> double) <*> (skipSpace *> double))
  <|> ("ONEMORE" *> pure OneMore)
  <|> ("BYE" *> pure Bye)

getCommand :: IO Command
getCommand = do
  t <- Text.getLine
  case parseOnly parseCommand t of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right c -> return c

time t tot k = min t $ tot / fromIntegral (34 - k)

player :: Player -> IO ()
player p = do
  putStrLn "RDY"
  hFlush stdout
  c <- getCommand
  case c of
    HeDid t tot c -> loop p white (time t tot 0) 0 (change black initial c)
    UGo t _ -> loop p black (min 0.1 t) 0 initial
    Bye -> exitSuccess
    OneMore -> return ()
  player p

loop :: Player -> Color -> Double -> Int -> Grid -> IO ()
loop p col t k g = do
  hPutStrLn stderr $ "rounds: " ++ show k
  g' <- if hasMove col g then do
    m <- p t col g
    putStrLn $ "IDO " ++ show (fst m) ++ " " ++ show (snd m)
    return $ change col g m
  else do
    putStrLn "IDO -1 -1"
    return g
  hFlush stdout
  c <- getCommand
  case c of
    HeDid t tot (-1, -1) -> loop p col (time t tot k) k g'
    HeDid t tot coord -> loop p col (time t tot k) (k + 1) (change (other col) g' coord)
    OneMore -> player p
    Bye -> exitSuccess