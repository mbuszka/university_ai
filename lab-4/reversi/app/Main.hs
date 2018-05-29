module Main where

import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))

import Tester
import MinMax
import Reversi
import Player

data AppMode = Bench | Test Ctx | Play 

modeP :: Parser Mode
modeP = (Timed <$> option auto (long "timed" <> short 't' <> metavar "TIME" <> help "Each decision will use up to TIME ms"))
  -- <|> (Iter <$> option auto (long "iter" <> short 'i' <> metavar "N" <> help "Each decision will run N progressively deeper iterations"))
  <|> (Fixed <$> option auto (long "fixed" <> short 'f' <> metavar "N" <> help "Each decision will reach depth up to N"))
  <|> (pure $ Timed 500)

ctxP :: Parser Ctx
ctxP = Ctx <$> modeP <*> pure False <*> pure eval'

appP :: Parser AppMode
appP = subparser 
  (  command "bench" (info (pure Bench) (progDesc "benchmark engine, by playing random games"))
  <> command "test" (info (Test <$> ctxP) (fullDesc <> progDesc "test the agents, by playing against random opponent"))
  <> command "play" (info (pure Play) (progDesc "play in the tournament"))
  )

main :: IO ()
main = do
  mode <- execParser opts
  case mode of
    Bench -> bench
    Test c -> test c
    Play -> player
  where
    opts = info (appP <**> helper)
      ( fullDesc
      <> header "reversi - game engine and some actors" )
      
  -- trains 1000 (Vec.replicate 64 0)
