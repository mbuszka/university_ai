module Reversi where

import qualified Data.Vector.Generic as Vec
import qualified Data.Vector         as BVec
import qualified Data.Vector.Unboxed as UVec

import Engine

type Agent = Grid -> IO Coord
type Players = Color -> Agent

posInf :: Double
posInf = 1 / 0

negInf :: Double
negInf = - posInf

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
  let w = length $ moves white g
      b = length $ moves black g
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

data Stat = Stat
  { whiteWon :: Int
  , blackWon :: Int
  , gamesTot :: Int
  } deriving (Eq, Ord, Show)

instance Monoid Stat where
  mappend (Stat a b c) (Stat a' b' c') = Stat (a + a') (b + b') (c + c')
  mempty = Stat 0 0 0

winner :: Grid -> Stat
winner g = case uncurry compare $ count g of
  LT -> Stat 1 0 1
  EQ -> Stat 0 0 1
  GT -> Stat 0 1 1

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

turn :: Color -> Players -> Grid -> IO (Maybe Grid)
turn c p grid = do
  (m, grid') <- if hasMove c grid
    then (,) True . change c grid <$> p c grid else return (False, grid)
  if hasMove (other c) grid'
    then Just . change (other c) grid' <$> p (other c) grid'
    else if m
      then return (Just grid')
      else return Nothing

game :: Color -> Players -> Grid -> IO Grid
game c p init = loop init
  where
    loop t = do
      mt <- turn c p t
      case mt of
        Just t' -> loop t'
        Nothing -> return t