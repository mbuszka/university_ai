module MCTS 
  ( mctsAgent
  ) where

import Engine (Grid, moves, hasMove, change)
import Reversi 
import RandomPlayer


import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Function
import Data.Int
import Data.Maybe
import Data.Monoid ((<>))
import System.Timeout

import qualified Data.Vector as Vec

data Color = White | Black
  deriving (Eq, Ord, Show)

other :: Color -> Color
other White = Black
other Black = White

toReversi :: Color -> Int8
toReversi White = 1
toReversi Black = -1

fromReversi c = if c == 1 then White else Black

data Tree
  = Node (Color, Stat) [Tree]
  | Leaf Color Stat Grid
  deriving (Eq, Ord, Show)
  -- | Terminal Color Grid

data TreeZ
  = Top
  | Path (Color, Stat) TreeZ [Tree] [Tree]

type Location = (Tree, TreeZ)

stat :: Tree -> Stat
stat t = case t of
  Leaf _ s _   -> s
  Node (_, s) _-> s

color :: Tree -> Color
color t = case t of
  Leaf c _ _    -> c
  Node (c, _) _ -> c

won :: Color -> Stat -> Int
won White = whiteWon
won Black = blackWon

score :: Int -> Color -> Tree -> Double
score n c t = let
  s = stat t
  tot = fromIntegral $ gamesTot s
  in fromIntegral (won c s) / tot + sqrt (2 * log (fromIntegral n / tot))

top :: Tree -> Location
top t = (t, Top)

left :: Location -> Maybe Location
left (t, Path s p (l:ls) rs) = Just (l, Path s p ls (t:rs))
left _ = Nothing

right :: Location -> Maybe Location
right (t, Path s p ls (r:rs)) = Just (r, Path s p (t:ls) rs)
right _ = Nothing

down :: Location -> Maybe Location
down (Node s (c:cs), p) = Just (c, Path s p [] cs)
down _ = Nothing

up :: Location -> Maybe Location
up (t, Path s p ls rs) = Just (Node s (reverse ls ++ (t:rs)), p)
up _ = Nothing

findBest :: Int -> Color -> Location -> Location
findBest n c loc = let
    aux loc@(t, _) val = let
        s = score n c t
        val' = case val of
          Just (best, bestScore) -> if s > bestScore then Just (loc, s) else val
          Nothing -> Just (loc, s)
      in case right loc of
        Just loc' -> aux loc' val'
        Nothing -> fst . fromJust $ val'
  in aux loc Nothing

select :: Int -> Location -> Location
select _ l@(Leaf _ _ _, p) = l
select n l@(t, _) = select n . findBest n (color t) . fromJust . down $ l

apply :: Stat -> Tree -> Tree
apply wins (Leaf c s g) = Leaf c (wins <> s) g
apply wins (Node (c, s) gs) = Node (c, wins <> s) gs

expand :: Location -> IO (Location, Stat)
expand (Leaf col s g, p) = do
  let c = toReversi col
  (t', scores) <- if not . hasMove c $ g
    then let r = winner g in return (Leaf col (r <> s) g, r)
    else do
      let
        rank g = do
          let c = toReversi $ other col
          g' <- game c play g
          let r = winner g'
          return (Leaf (other col) r g, r)
        children = Vec.map (change c g) $ moves c g
      scores <- Vec.mapM rank children
      let
        (ts, rs) = Vec.foldr (\(t, r) (ts, rs) -> (t:ts, r <> rs)) ([], mempty) scores
      return (Node (col, (s <> rs)) ts, rs)
  return ((t', p), scores)
      
backup :: Stat -> Location -> (Tree, Int)
backup s (t, Top) = (t, gamesTot s)
backup r l = 
  let Just (t, p) = up l
  in backup r (apply r t, p)

mcts :: Int -> Tree -> IO (Tree, Int)
mcts n t = do
  let node = select n (top t)
  (l, r) <- expand node
  return $ backup r l

loop :: Int -> TVar Tree -> Tree -> IO ()
loop n var t = do
  (t', k) <- mcts n t
  atomically $ writeTVar var t'
  loop (n + k) var t'

mctsAgent :: Double -> Int8 -> Agent
mctsAgent secs col grid = do
  let c = fromReversi col
  let t = Leaf c mempty grid
  var <- atomically $ newTVar t
  -- putStrLn "searching"
  let tm = floor $ secs * 1000000 - 10000
  timeout tm (loop 0 var t)
  -- putStrLn "timed out"
  Node _ cs <- atomically $ readTVar var
  let cts = Vec.zip (moves col grid) (Vec.fromList cs)
      val (_, t) = let s = stat t in fromIntegral (gamesTot s)
      best = fst $ Vec.maximumBy (compare `on` val) cts
  return best