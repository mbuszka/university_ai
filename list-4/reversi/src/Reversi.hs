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

import Grid

type Ply = (Grid, NonEmpty Grid)

dirs :: [Dir]
dirs = [ N, NE, E, SE, S, SW, W, NW ]

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

score = finalScore

finalScore :: Grid -> Double
finalScore g =
  let (b, w) = count g in
  if (b < w) 
    then fromIntegral w / fromIntegral (b + w) + 1 / 2
    else negate $ fromIntegral b / fromIntegral (b + w) - 1 / 2

evalPositions :: Grid -> Double
evalPositions g =
  let s = Vec.foldl' (+) 0 . Vec.zipWith (*) weights . Vec.map fromIntegral $ g
  in s / 82

eval :: Grid -> Double
eval g = finalScore g + evalPositions g

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
