module Reversi where

import qualified Data.Vector.Generic as Vec
import qualified Data.Vector         as BVec
import qualified Data.Vector.Unboxed as UVec

import Engine

data Tree = Tree 
  { grid :: Grid
  , whiteM :: BVec.Vector (Coord, Tree)
  , blackM :: BVec.Vector (Coord, Tree)
  }

posInf :: Double
posInf = 1 / 0

negInf :: Double
negInf = - posInf

isTerminal :: Tree -> Bool
isTerminal (Tree _ w b) = Vec.null w && Vec.null b

hasMoves :: Color -> Tree -> Bool
hasMoves c (Tree _ wm bm) = if c == white
  then not $ null wm
  else not $ null bm

gameTree :: Grid -> Tree
gameTree start = Tree start (fmap (\(c, t) -> (c, gameTree t)) $ moves start white) (fmap (\(c, t) -> (c, gameTree t)) $ moves start black)

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
        where
          eval :: UVec.Vector Double -> Grid -> Double
          eval w g = -- evalCorners g + evalPositions g + evalPosession g
              (w Vec.! 0) * evalPositions g
            + (w Vec.! 1) * evalPosession g
            -- + (w Vec.! 2) * evalFrontier g

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

oneTurn :: (Tree -> IO Tree) -> (Tree -> IO Tree) -> Tree -> IO (Maybe Tree)
oneTurn w b t = do
  (m, t') <- if hasMoves white t
    then (,) True <$> w t else return (False, t)
  if hasMoves black t' 
    then Just <$> b t' 
    else if m 
    then return (Just t')
    else return Nothing

oneGame :: Grid -> (Tree -> IO Tree) -> (Tree -> IO Tree) -> IO Grid
oneGame init w b = loop (gameTree init)
  where
    loop t = do
      mt <- oneTurn w b t
      case mt of
        Just t' -> loop t'
        Nothing -> return $ grid t