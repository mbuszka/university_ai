{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Hashable
import Data.HashMap.Strict as Map
import Data.List as List
import Data.Sequence as Seq

import System.IO.Unsafe

type Pos = (Int, Int)

data Color = White | Black
  deriving (Eq, Ord, Generic, Show)

instance Hashable Color

data State = State 
  { whiteTower :: Pos
  , whiteKing  :: Pos
  , blackKing  :: Pos 
  }
  deriving (Eq, Ord, Generic, Show)

instance Hashable State

next :: Color -> Color
next Black = White
next White = Black

moves :: Color -> State -> [State]
moves Black (State wt wk bk) = List.filter isValid $ fmap (\k -> State wt wk k) $ kingNeighbours bk
moves White (State wt wk bk) = List.filter isValid $ fmap (\k -> State wt k bk) (kingNeighbours wk) ++
  fmap (\t -> State t wk bk) (towerNeighbours wt)

check :: State -> Bool
check (State (tx, ty) _ (kx , ky)) = tx == kx || ty == ky

checkMate :: State -> Bool
checkMate s = all check $ moves Black s

isValid :: State -> Bool
isValid (State wt wk bk) = wt /= wk && wt /= bk && wk /= bk && all (/= wk) (kingNeighbours bk)

inRange :: Int -> Bool
inRange x = x >= 1 && x <= 8

kingNeighbours :: Pos -> [Pos]
kingNeighbours (x, y) = [(a, b) | a <- [x - 1 .. x + 1],
                                  b <- [y - 1 .. y + 1],
                                  inRange a, inRange b]

towerNeighbours :: Pos -> [Pos]
towerNeighbours (x, y) = [(x, z) | z <- [1 .. 8]] ++ [(z, y) | z <- [1 .. 8]]

strToPos :: String -> Pos
strToPos [x, y] = ((fromEnum x) - (fromEnum 'a') + 1, read [y])
strToPos _ = error "Unexpected input"

parseInput :: String -> (Color, State)
parseInput s =  
  let [c, wk, wt, bk] = words s
  in (if c == "black" then Black else White, State (strToPos wt) (strToPos wk) (strToPos bk))

choose :: [State] -> [State] -> [State]
choose s1 s2 = if List.length s1 < List.length s2 then s1 else s2

run :: Seq (Color, [State]) -> HashMap (Color, State) [State] -> Maybe [State] -> Maybe [State]
run q visited best = case Seq.viewl q of
  EmptyL -> best
  (c, s:ss) :< q ->
    let
      ms = moves c s
      q' = List.foldl (\q s' -> if Map.member (next c, s') visited then q else q |> (next c, s':s:ss)) q ms
      v' = List.foldl (\v s' -> Map.insertWith choose (next c, s') (s':s:ss) v) visited ms
      b = if checkMate s
        then case best of
          Nothing -> Just $ s:ss
          Just s' -> Just $ choose s' (s:ss)
        else best
    in -- seq (unsafePerformIO $ print (Map.size v'))
      run q' v' b



main :: IO ()
main = putStrLn "Hello world"