-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}
module Grid where

import           Data.Functor.Identity
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array as Arr
import           Data.Array (Ix, Array)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Int
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import           Data.Vector.Unboxed (Vector)
import qualified Streaming.Prelude as S

type Color = Int8
type Coord = (Int, Int)
type Grid = Vector Color

-- instance Hashable (Vector Color)

data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Ix, Eq, Ord, Show)


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

step :: Dir -> Coord -> Coord
step d c = plus c (diff d)

neighbours :: Coord -> [Coord]
neighbours c = filter goodCoord . map (plus c . diff) $ dirs

hasEmptyNeighbour :: Grid -> Coord -> Bool
hasEmptyNeighbour g = any ((0 ==) . (g Vec.!) . toLin) . neighbours

goodCoord :: Coord -> Bool
goodCoord (x, y) = 0 <= x && x < 8 && 0 <= y && y < 8 

-- indices :: Array (Int, Int, Dir) (Vector Int)
-- indices = Arr.array ((0, 0, N), (7, 7, NW)) $ 
--   [ ((x, y, d), gen x y d) | x <- [0 .. 7], y <- [0 .. 7], d <- dirs ]

-- gen :: Int -> Int -> Dir -> Vector Int
-- gen x y d = Vec.unfoldr (\c -> if goodCoord c then Just (toLin c, step d c) else Nothing) (x, y)

gen :: Int -> Int -> Dir -> S.Stream (S.Of Int) Identity ()
gen x y d = S.map toLin . S.takeWhile goodCoord $ S.iterate (step d) (x, y)

-- applyMove :: Color -> Coord -> Grid -> Grid
-- applyMove c (x, y) g =
--   let
--     one d v = do
--       let is = indices Arr.! (x, y, d)
--       c <- MVec.read v (is Vec.! 0)


-- allIndices :: Map (Coord, Dir) [Int]
-- allIndices = Map.fromList $
--   [ (((x, y), d), gen (x, y) d) | x <- [0 .. 7], y <- [0 .. 7], d <- dirs]

-- indices c d = allIndices Map.! (c, d)

-- gen :: Coord -> Dir -> [Int]
-- gen c d =
--   let cs = map toLin . takeWhile goodCoord . iterate (plus . diff $ d) $ c
--   in cs

{-# INLINE is #-}
is :: Color -> Grid -> Coord -> Bool
is col g coord = g Vec.! (toLin coord) == col

when :: (a -> Bool) -> a -> Maybe a
when p x
  | p x = Just x
  | otherwise = Nothing

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
whileM p f x = if p x then whileM p f =<< (f x) else return x

{-# INLINE vHead #-}
vHead v = if Vec.null v then Nothing else Just $ Vec.unsafeHead v

{-# INLINE vTail #-}
vTail v = if Vec.null v then Nothing else Just $ Vec.unsafeTail v

{-# INLINE vUncons #-}
vUncons v = if Vec.null v then Nothing else Just (Vec.unsafeHead v, Vec.unsafeTail v)

checkPath :: Color -> Grid -> Coord -> Dir -> Bool
checkPath col g (x, y) d = isJust $ do
  let is = gen x y d
  (h, t) <- runIdentity $ S.uncons is
  guard (g `Vec.unsafeIndex` h == empty)
  n <- runIdentity $ S.head_ t
  guard (g `Vec.unsafeIndex` n == (other col))
  let rest = S.dropWhile (\i -> g `Vec.unsafeIndex` i == (other col)) t
  l <- runIdentity $ S.head_ rest
  guard (g `Vec.unsafeIndex` l == col)

indices :: Color -> Grid -> Coord -> Dir -> S.Stream (S.Of Int) Identity ()
indices col g (x, y) d =
  S.cons (toLin (x, y)) .
  S.takeWhile (\i -> g `Vec.unsafeIndex` i == (other col)) .
  S.drop 2 $
  gen x y d  

toChange :: Color -> Grid -> Coord -> S.Stream (S.Of Int) Identity ()
toChange col g coord = flip S.for (indices col g coord) . S.filter (checkPath col g coord) $ S.each dirs


-- path :: Color -> Grid -> Coord -> [Vector Int]
-- path col g (x, y) = (Vec.singleton $ toLin (x, y)) : mapMaybe check dirs where
--   check d = 
--     let is = gen x y d
--     in {-# SCC path_do #-} do
--       (h, t) <- runIdentity $ S.uncons is
--       guard (g `Vec.unsafeIndex` h == empty)
--       n <- runIdentity $ S.head_ t
--       guard (g `Vec.unsafeIndex` n == (other col))
--       let (good, rest) = S.spa (\i -> g `Vec.unsafeIndex` i == (other col)) t
--       l <- vHead rest
--       guard (g `Vec.unsafeIndex` l == col)
--       return good

    -- let step c = when goodCoord $ plus c $ diff d
    --     opp = other col
    -- in do
    --   c1 <- when (is empty g) coord >>= step
    --   c2 <- when (is opp g) c1 >>= step
    --   c3 <- whileM (is opp g) step c2
    --   when (is col g) c3


-- path :: Coord -> Dir -> Grid -> [(Int, Color)]
-- path (x, y) d g = map (\i -> (i, g Vec.! i)) $ Vec.toList $ indices Arr.! (x, y, d)

emptyTiles :: Grid -> S.Stream (S.Of Coord) Identity ()
emptyTiles = S.map fromLin . S.each . Vec.toList . Vec.elemIndices 0

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
