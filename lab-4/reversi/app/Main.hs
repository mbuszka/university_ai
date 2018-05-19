module Main where

import qualified Data.Vector.Unboxed as Vec
import System.Environment
import Tester
import MinMax

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
    ctx = Ctx mode doSort (Vec.fromListN 3 [502.78, 19.59, 431.02])
  case comStr of
    "test" -> test ctx
    -- "train" -> runTraining ctx

      
  -- trains 1000 (Vec.replicate 64 0)
