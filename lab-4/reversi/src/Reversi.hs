module Reversi where

import Data.Functor.Identity
import           Data.Int
import qualified Data.List as List
import qualified Data.List.NonEmpty as NL
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe as Maybe
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Vector.Generic as Vec
import qualified Data.Vector         as BVec
import qualified Data.Vector.Unboxed as UVec

import Grid

data Tree = Tree 
  { grid :: Grid
  , whiteM :: BVec.Vector Tree
  , blackM :: BVec.Vector Tree
  }

posInf :: Double
posInf = 1 / 0

negInf :: Double
negInf = - posInf

moves :: Grid -> Color -> BVec.Vector Grid
moves g col =
  let good = Vec.filter (check col g) $ emptyTiles g
      grids = Vec.map (change col g) good
  in grids

isTerminal :: Tree -> Bool
isTerminal (Tree _ w b) = Vec.null w && Vec.null b

hasMoves :: Color -> Tree -> Bool
hasMoves c (Tree _ wm bm) = if c == white
  then not $ null wm
  else not $ null bm

gameTree :: Grid -> Tree
gameTree start = Tree start (gameTree <$> moves start white) (gameTree <$> moves start black)

evalPosession :: Grid -> Double
evalPosession g =
  let (b, w) = count g in
  if b < w
    then fromIntegral w / fromIntegral (b + w) + 1 / 2
    else negate $ fromIntegral b / fromIntegral (b + w) + 1 / 2

evalPositions :: Grid -> Double
evalPositions g =
  let s = Vec.foldl' (+) 0 . Vec.zipWith (*) weights . Vec.map fromIntegral $ g
  in s / 82

evalMobility :: Grid -> Double
evalMobility g =
  let w = length $ moves g white
      b = length $ moves g black
  in if b < w 
    then fromIntegral w / fromIntegral (w + b) + 1/2
    else negate $ fromIntegral b / fromIntegral (b + w) + 1 / 2

evalFrontier :: Grid -> Double
evalFrontier g = 
  let (b, w) = count . Vec.imap (\c v -> if hasEmptyNeighbour g $ fromLin c then v else 0) $ g
  in if b < w
    then negate $ fromIntegral w / fromIntegral (b + w) + 1 / 2
    else fromIntegral b / fromIntegral (b + w) + 1 / 2

finalScore :: Grid -> Double
finalScore g = let (b, w) = count g
  in case b `compare` w of
    LT -> posInf
    EQ -> 0
    GT -> negInf

evalCorners :: Grid -> Double
evalCorners g = let
  ix = [ (0, [1, 8])
       , (7, [6, 15])
       , (56, [48, 57])
       , (63, [62, 55])
       ]
  f (c, [a, b]) = if g Vec.! c == 0 
    then 2 * (g Vec.! a) + 2 * (g Vec.! b)
    else 10 * (g Vec.! c)
  in fromIntegral $ sum $ map f ix

eval' = eval (Vec.fromListN 3 [502.78, 19.59, 431.02])

eval :: UVec.Vector Double -> Grid -> Double
eval w g = -- evalCorners g + evalPositions g + evalPosession g
    (w Vec.! 0) * evalPositions g
  + (w Vec.! 1) * evalPosession g
  + (w Vec.! 2) * evalFrontier g

upperLeft :: [[Double]]
upperLeft =
  [ [ 16.0, -3.0, 1.0, 0.5 ]
  , [ -4  , -1.8, 0  , 0 ]
  , [ 1.3 , 0   , 0.5, 0 ]
  , [ 0.5 , 0   , 0  , 0 ]
  ]

weights =
  let y = map (\x -> x ++ reverse x) upperLeft
  in Vec.fromList $ concat $ y ++ reverse y
