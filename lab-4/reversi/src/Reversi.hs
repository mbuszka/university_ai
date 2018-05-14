module Reversi where

import           Data.Int
import qualified Data.List as List
import qualified Data.List.NonEmpty as NL
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe as Maybe
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Vector.Unboxed as Vec
import           Data.Vector.Unboxed (Vector)

import Grid

type Ply = (Grid, NonEmpty Grid)

posInf :: Double
posInf = 1 / 0

negInf :: Double
negInf = - posInf

paths :: Coord -> Grid -> [[(Int, Color)]]
paths c g = [path c d g | d <- dirs]

goodPath :: Color -> [(Int, Color)] -> Set Int
goodPath c [_] = Set.empty
goodPath c [_, _] = Set.empty
goodPath c (this:next:rest) =
  if snd next == other c
     && sandwich
  then Set.fromList $ map fst (this:next:toFlip)
  else Set.empty
  where
    (toFlip, tail) = span (\x -> snd x == other c) rest
    sandwich = Just c == (fmap (snd . fst) . List.uncons $ tail)

goodPos :: Coord -> Color -> Grid -> Set Int
goodPos coord color g =
  foldr (Set.union . goodPath color) Set.empty (paths coord g)

moves :: Grid -> Color -> [Grid]
moves g c =
  let good = filter (not . Set.null) $ fmap (\coord -> goodPos coord c g) $ emptyTiles g
      grids = map (changeTiles g c . Set.toList) good
  in grids

end :: Grid -> Bool
end g = null (moves g black) && null (moves g white)

ply :: Grid -> Color -> Maybe Ply
ply g c = ((,) g) <$> NL.nonEmpty (moves g c)

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

-- evalCorners :: Grid -> Double
-- evalCorners g = fromIntegral (sum $ map (g Vec.!) [ 0, 7, 56, 63]) / 4

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

eval :: Vector Double -> Grid -> Double
eval w g = evalCorners g
  --   (w Vec.! 0) * evalPositions g
  -- + (w Vec.! 1) * evalPosession g
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

  -- weights = Map.fromList $
--      upperLeft 
--   ++ map (\(C x y, w) -> (C (7 - x) y, w)) upperLeft
--   ++ map (\(C x y, w) -> (C x (7 - y), w)) upperLeft
--   ++ map (\(C x y, w) -> (C y x, w)) upperLeft

-- eval :: Color -> Grid -> Double
-- eval c g = 
--   sum $ [fromMaybe 0.0 $ Map.lookup (C x y) weights |
--           x <- [0..7]
--         , y <- [0..7]
--         , Map.lookup (C x y) g == Just c
--         ]
