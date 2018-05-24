module Engine 
  ( Grid (..)
  , Color (..)
  , Coord (..)
  , Dir (..)
  , moves
  , hasMove
  , change
  , black
  , white
  , empty
  , other
  , count
  , hasEmptyNeighbour
  , initial
  , toLin, fromLin
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Int
import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Unboxed.Mutable as MVec
import qualified Data.Vector                 as BVec
import qualified Data.Vector.Generic         as Vec

type Color = Int8
type Coord = (Int, Int)
type Grid  = UVec.Vector Color

data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Enum, Eq, Ord, Show)


dirs :: BVec.Vector Dir
dirs = Vec.fromListN 8 [ N, NE, E, SE, S, SW, W, NW ]

deltas :: UVec.Vector Coord
deltas = Vec.convert $ Vec.map diff dirs

white :: Color
white = 1

black :: Color
black = -1

empty :: Color
empty = 0

{-# INLINE other #-}
other :: Color -> Color
other c = - c

{-# INLINE toLin #-}
toLin :: Coord -> Int
toLin (x, y) = 8 * x + y

{-# INLINE fromLin #-}
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

{-# INLINE diff #-}
diff :: Dir -> Coord
diff N  = (1, 0)
diff NE = (1, 1)
diff E  = (0, 1)
diff SE = (-1, 1)
diff S  = (-1, 0)
diff SW = (-1, -1)
diff W  = (0, -1)
diff NW = (1, -1)

{-# INLINE plus #-}
plus :: Coord -> Coord -> Coord
plus (x, y) (dx, dy) = (x + dx, y + dy)

{-# INLINE step #-}
step :: Dir -> Coord -> Coord
step d c = plus c (diff d)

neighbours :: Coord -> UVec.Vector Coord
neighbours c = Vec.filter goodCoord . Vec.map (plus c) $ deltas

hasEmptyNeighbour :: Grid -> Coord -> Bool
hasEmptyNeighbour g = Vec.any ((0 ==) . (g Vec.!) . toLin) . neighbours

{-# INLINE goodCoord #-}
goodCoord :: Coord -> Bool
goodCoord (x, y) = 0 <= x && x < 8 && 0 <= y && y < 8 

{-# INLINE is #-}
is :: Color -> Grid -> Coord -> Bool
is col g x = g `Vec.unsafeIndex` (toLin x) == col

{-# INLINE checkPath #-}
checkPath :: Color -> Grid -> Coord -> Dir -> Bool
checkPath col g c0 d =
  let c = step d c0
  in if not (goodCoord c) || (not $ is (other col) g c) then False
  else
    let c' = loop c
    in if not (goodCoord c') || (not $ is col g c') then False
    else True
  where
    loop c =
      if not (goodCoord c) || (not $ is (other col) g c) then c
      else loop (step d c)

{-# INLINE check #-}
check :: Color -> Grid -> Coord -> Bool
check col g c = BVec.any (checkPath col g c) dirs

{-# INLINE changeD #-}
changeD :: Color -> Coord -> MVec.MVector s Int8 -> Dir -> (ST s (MVec.MVector s Int8))
changeD col c0 g d = do
  let c' = step d c0
  MVec.unsafeWrite g (toLin c0) col
  loop c'
  return g
  where
    loop c = do
      v <- MVec.unsafeRead g (toLin c)
      if v /= (other col) then return ()
      else do
        MVec.unsafeWrite g (toLin c) col
        loop (step d c)

{-# INLINE change #-}
change :: Color -> Grid -> Coord -> Grid
change col g c = 
  let 
    comp = do
      v <- Vec.thaw g
      v' <- Vec.foldM (changeD col c) v $ Vec.filter (checkPath col g c) dirs
      Vec.freeze v'
  in runST comp 

{-# INLINE emptyTiles #-}
emptyTiles :: Grid -> BVec.Vector Coord
emptyTiles = Vec.map fromLin . Vec.convert . Vec.elemIndices 0

{-# INLINE changeTiles #-}
changeTiles :: Color -> Grid -> UVec.Vector Int -> Grid
changeTiles color grid is = Vec.update grid (Vec.map (\i -> (i, color)) is)

{-# INLINEABLE moves #-}
moves :: Color -> Grid -> BVec.Vector Coord
moves color grid = Vec.filter (check color grid) $ emptyTiles grid

{-# INLINEABLE hasMove #-}
hasMove :: Color -> Grid -> Bool
hasMove color grid = Vec.any (check color grid) $ emptyTiles grid

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
