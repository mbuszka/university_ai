module Main where

import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))

import Bench
import Tester
import MinMax
import MCTS
import RandomPlayer
import Reversi
import Player

data AppMode = Bench | Test Agent Agent | Play Player

mctsHelp = help "Monte Carlo Tree Search Agent"
timedHelp = help "Timed MinMax Agent"
fixedHelp = help "Fixed depth MinMax Agent"
randomHelp = help "Random choice Agent"

agentP :: Parser Agent
agentP = 
      (mctsAgent <$> option auto (long "mcts" <> short 'm' <> metavar "TIME" <> mctsHelp))
  <|> (timed <$> option auto (long "timed" <> short 't' <> metavar "TIME" <> timedHelp))
  <|> (fixed <$> option auto (long "fixed" <> short 'f' <> metavar "DEPTH" <> fixedHelp))
  <|> (pure play <* flag' () (long "random" <> short 'r' <> randomHelp))

appP :: Parser AppMode
appP = subparser 
  (  command "bench" 
      (info
        (pure Bench) 
        (fullDesc <> progDesc "Benchmark engine by playing random games"))
  <> command "test" 
      (info
        (Test <$> agentP <*> agentP)
        (fullDesc <> progDesc "Test agents against each other"))
  <> command "play"
      (info 
        (Play <$> 
          (   pure mctsAgent <* flag' () (long "mcts" <> short 'm' <> mctsHelp)
          <|> pure timed <* flag' () (long "timed" <> short 't' <> timedHelp)
          ))
        (fullDesc <> progDesc "Tournament player"))
  )

main :: IO ()
main = do
  mode <- execParser opts
  case mode of
    Bench -> bench
    Test p1 p2 -> test p1 p2 
    Play p -> player p
  where
    opts = info (appP <**> helper)
      ( fullDesc
      <> progDesc "Reversi"
      <> header "reversi - game engine and actors" )
      
  -- trains 1000 (Vec.replicate 64 0)
