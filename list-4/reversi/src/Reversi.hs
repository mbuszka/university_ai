module Reversi where

import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)

data Color = Black | White
  deriving (Eq, Ord, Show)

data Coord = C !Int !Int
  deriving (Eq, Ord, Show)
type Grid = Map Coord Color

data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Show)

other :: Color -> Color
other White = Black
other Black = White

showGrid :: Grid -> String
showGrid g = let
  p = [[pix (Map.lookup (C i j) g) | j <- [0..7]] | i <- [0..7]]
  pix Nothing = '.'
  pix (Just White) = 'W'
  pix (Just Black) = 'B'
  in unlines p

dirs :: [Dir]
dirs = [ N, NE, E, SE, S, SW, W, NW ]

diff :: Dir -> Coord
diff N  = C 1 0
diff NE = C 1 1
diff E  = C 0 1
diff SE = C (-1) 1
diff S  = C (-1) 0
diff SW = C (-1) (-1)
diff W  = C 0 (-1)
diff NW = C 1 (-1)

{-# INLINE plus #-}
plus :: Coord -> Coord -> Coord
plus (C x y) (C dx dy) = C (x + dx) (y + dy)

goodCoord :: Coord -> Bool
goodCoord (C x y) = 0 <= x && x <= 7 && 0 <= y && y <= 7

-- neighbours :: Coord -> [Coord]
-- neighbours c = filter goodCoord $ fmap (plus c . diff) dirs

paths :: Coord -> [(Dir, [Coord])]
paths c = [(d, iterate (plus $ diff d) c) | d <- dirs]

goodPath :: Grid -> Color -> [Coord] -> Set Coord
goodPath g color (s:c:cs) =
  if maybe False (/= color) (Map.lookup c g)
     && sandwich
  then Set.fromList $ s:c:toFlip
  else Set.empty
  where
    (toFlip, rest) = span (maybe False (/= color) . flip Map.lookup g) cs
    sandwich = Just color == Map.lookup (head rest) g

goodPos :: Grid -> Color -> Coord -> Set Coord
goodPos g color c = if goodCoord c 
  then foldr (Set.union . goodPath g color) Set.empty (fmap snd $ paths c)
  else Set.empty

moves :: Grid -> Color -> [Grid]
moves g c =
  let coords = Set.fromList [C i j | i <- [0..7], j <- [0..7]] `Set.difference` Map.keysSet g
      good = filter (not . Set.null) $ fmap (goodPos g c) $ Set.toList coords
      grids = map (foldr (flip Map.insert c) g) good
  in grids

initial :: Grid
initial = Map.fromList
  [ (C 3 3, White)
  , (C 3 4, Black)
  , (C 4 3, Black)
  , (C 4 4, White)
  ]

end :: Grid -> Bool
end g = null (moves g Black) && null (moves g White)

score :: Color -> Grid -> Double
score c g = let
  (my, enemy) = partition (== c) $ Map.elems g
  in fromIntegral $ length my - length enemy

upperLeft :: [(Coord, Double)]
upperLeft =
  [ (C 0 0, 16.0)
  , (C 0 1, -3.0)
  , (C 0 2, 1.0)
  , (C 0 3, 0.5)
  , (C 1 0, -4)
  , (C 1 1, -1.8)
  , (C 1 2, 0)
  , (C 1 3, 0)
  , (C 2 0, 1.3)
  , (C 2 1, 0)
  , (C 2 2, 0.5)
  , (C 2 3, 0)
  , (C 3 0, 0.5)
  , (C 3 1, 0)
  , (C 3 2, 0)
  , (C 3 3, 0)
  ]

weights = Map.fromList $
     upperLeft 
  ++ map (\(C x y, w) -> (C (7 - x) y, w)) upperLeft
  ++ map (\(C x y, w) -> (C x (7 - y), w)) upperLeft
  ++ map (\(C x y, w) -> (C y x, w)) upperLeft

eval :: Color -> Grid -> Double
eval c g = 
  sum $ [fromMaybe 0.0 $ Map.lookup (C x y) weights |
          x <- [0..7]
        , y <- [0..7]
        , Map.lookup (C x y) g == Just c
        ]
