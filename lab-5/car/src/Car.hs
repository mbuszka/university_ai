module Car where

import Data.Function
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map as Map


data State = State
  { _x :: Int
  , _y :: Int
  , _vx :: Int
  , _vy :: Int
  } deriving (Eq, Ord, Show)

data Action  = Slip | Accelerate Int Int
data Surface = Field | Road | Oil | Finish

type Track = Vector (Vector Char)
type Score = Map.Map State (Action, Double)

{-# INLINE addBounded #-}
addBounded :: (Int, Int) -> Int -> Int -> Int
addBounded (l, u) x dx = min u . max l $ x + dx

surface :: Track -> Int -> Int -> Surface
surface t x y = case t Vec.!? y of
  Nothing -> Field
  Just v -> case v Vec.!? x of
              Just '#' -> Road
              Just 'o' -> Oil
              Just 's' -> Road
              Just 'e' -> Finish
              _        -> Field

initial :: Track -> Score
initial t = Vec.ifoldl' 
  (\a y v ->
    Vec.ifoldl' 
      (\a x c -> 
        if c /= '.'
          then foldl
                (\a k -> Map.insert k (Accelerate 0 0, 0) a)
                a 
                [ State x y vx vy 
                | vx <- [-3 .. 3]
                , vy <- [-3 .. 3]
                ]
          else a
      ) a v
  ) Map.empty t

merge :: Action -> Action -> Action
merge (Accelerate x y) (Accelerate x' y') = Accelerate (x + x') (y + y')

apply :: Action -> State -> State
apply (Accelerate dvx dvy) s = let
    vx' = addBounded (-3, 3) dvx $ _vx s
    vy' = addBounded (-3, 3) dvy $ _vy s
    x'  = _x s + vx'
    y'  = _y s + vy'
  in State x' y' vx' vy'

possibleMoves :: Vector Action
possibleMoves = Vec.fromList [Accelerate x y | x <- [-1 .. 1], y <- [-1 .. 1]]

actions :: Track -> State -> Vector Action
actions t s = case surface t (_x s) (_y s) of
  Road -> possibleMoves
  Oil  -> possibleMoves
  _    -> Vec.singleton Slip

moves :: Track -> State -> Action -> Vector (Double, State)
moves t s a = case surface t (_x s) (_y s) of
  Oil  -> Vec.map (\a' -> (1/9, apply (merge a a') s)) possibleMoves
  Road -> Vec.singleton (1, apply a s)
  _    -> Vec.empty

reward :: Track -> State -> Action -> Double
reward t s _ = case surface t (_x s) (_y s) of
  Field  -> negate 100
  Road   -> negate 0.1
  Oil    -> negate 0.1
  Finish -> 100

step :: Track -> Score -> Score
step t score = Map.mapWithKey 
  (\s _ ->
    Vec.map
      (\a ->
        let ss = Vec.map (\(p, s) -> p * (reward t s a + 0.99 * (maybe 0 snd $ Map.lookup s score))) $ moves t s a
        in (a, Vec.sum ss)
      ) (actions t s) & Vec.maximumBy (compare `on` snd)
  ) score

scores :: Int -> Track -> Score
scores n t = iterate (step t) (initial t) !! n
