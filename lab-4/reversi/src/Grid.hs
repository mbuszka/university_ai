-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}
module Grid where

-- import           Data.Hashable
import           Data.Int
import qualified Data.Vector.Unboxed as Vec
import           Data.Vector.Unboxed (Vector)

type Color = Int8
type Coord = (Int, Int)
type Grid = Vector Color

-- instance Hashable (Vector Color)

data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Show)


dirs :: [Dir]
dirs = [ N, NE, E, SE, S, SW, W, NW ]

white :: Color
white = 1

black :: Color
black = -1

empty :: Color
empty = 0

other :: Color -> Color
other c = - c

toLin :: Coord -> Int
toLin (x, y) = 8 * x + y

fromLin :: Int -> Coord
fromLin v = v `divMod` 8

showGrid :: Grid -> String
showGrid g = let
  toLines [] = []
  toLines (x:xs) = h:toLines t where
    (h, t) = splitAt 8 (x:xs)
  p = toLines . map pix . Vec.toList $ g
  pix 0 = '.'
  pix 1 = 'W'
  pix (-1) = 'B'
  in unlines p

diff :: Dir -> Coord
diff N  = (1, 0)
diff NE = (1, 1)
diff E  = (0, 1)
diff SE = (-1, 1)
diff S  = (-1, 0)
diff SW = (-1, -1)
diff W  = (0, -1)
diff NW = (1, -1)

plus :: Coord -> Coord -> Coord
plus (x, y) (dx, dy) = (x + dx, y + dy)

neighbours :: Coord -> [Coord]
neighbours c = filter goodCoord . map (plus c . diff) $ dirs

hasEmptyNeighbour :: Grid -> Coord -> Bool
hasEmptyNeighbour g = any ((0 ==) . (g Vec.!) . toLin) . neighbours

goodCoord :: Coord -> Bool
goodCoord (x, y) = 0 <= x && x < 8 && 0 <= y && y < 8 

indices :: Coord -> Dir -> [Int]
indices c d =
  let cs = map toLin . takeWhile goodCoord . iterate (plus . diff $ d) $ c
  in cs

path :: Coord -> Dir -> Grid -> [(Int, Color)]
path c d g = map (\i -> (i, g Vec.! i)) $ indices c d

emptyTiles :: Grid -> [Coord]
emptyTiles = map fromLin . Vec.toList . Vec.elemIndices 0

changeTiles :: Grid -> Color -> [Int] -> Grid
changeTiles g c is = g Vec.// (map (\i -> (i, c)) is)

initial :: Grid
initial = Vec.generate 64 (f . fromLin) where
  f (3, 3) = white
  f (3, 4) = black
  f (4, 3) = black
  f (4, 4) = white
  f _ = empty

count :: Grid -> (Int, Int)
count = Vec.foldl'
  (\(b, w) x -> case x of
    -1 -> (b+1, w)
    0  -> (b, w)
    1  -> (b, w + 1))
  (0, 0)
