module Main where

import Car

import qualified Data.Vector as Vec
import qualified Data.Map as Map
import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import System.IO

input :: Parser (String, String, Int)
input = (,,) 
  <$> strOption (long "if" <> help "input file" <> metavar "FILE")
  <*> strOption (long "of" <> help "output file" <> metavar "FILE")
  <*> option auto (long "iter" <> short 'n' <> help "iterations" <> value 100 <> showDefault <> metavar "N")

main :: IO ()
main = do
  (inf, outf, n) <- execParser $ info (input <**> helper) fullDesc
  ls <- lines <$> readFile inf
  let track = Vec.fromList $ map Vec.fromList ls
  let res = scores n track
  withFile outf WriteMode $ \handle -> do
    forM_ (Map.toList res) $ \(s, (a, v)) -> do
        let State x y vx vy = s
        case a of 
          Accelerate dx dy -> do
            hPutStrLn handle $ unwords $ map show [x, y, vx, vy, dx, dy]
          _ -> return ()
