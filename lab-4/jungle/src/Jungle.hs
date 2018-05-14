module Jungle where

import Control.Monad

import           Data.Function
import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Char (toUpper)

type Coord = (Int, Int)
type Player = Int
type State = Map Coord (Player, Animal)

other :: Player -> Player
other = negate

data Tile = Field | Trap | Pond | Cave Player
  deriving (Eq, Ord, Show)

data Animal = Rat | Cat | Dog | Wolf | Jaguar | Tiger | Lion | Elephant
  deriving (Eq, Ord, Show, Enum)

data Dir = N | E | S | W
  deriving (Eq, Ord, Show)
  
dirs :: [Dir]
dirs = [ N, E, S, W ]

beats :: Animal -> Animal -> Bool
beats Rat Elephant = True
beats a   b        = a >= b

goodCoord :: Coord -> Bool
goodCoord (x, y) = 
     0 <= x && x <= 8
  && 0 <= y && y <= 6

moveD :: Coord -> Dir -> Coord
moveD (x, y) d = case d of
  N -> (x - 1, y)
  S -> (x + 1, y)
  E -> (x, y + 1)
  W -> (x, y - 1)

dist :: Coord -> Coord -> Int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)

compareDist :: State -> Player
compareDist s =
  let (u, d) = partition ((==1) . fst . snd) $ Map.toList s
      u' = sort $ dist (8, 3) . fst <$> u
      d' = sort $ dist (0, 3) . fst <$> d
      t = dropWhile (uncurry (==)) $ zip u d
  in case t of
    [] -> -1
    (a, b):_ -> if a < b then 1 else -1
    
winner :: (State, Int) -> Maybe Player
winner (s, k)
  | k >= 50 =
    let (u, d) = partition ((==1) . fst) $ Map.elems s
    in case (u, d) of
      ([], _) -> Just (-1)
      (_, []) -> Just 1
      _ -> case maximum (snd <$> u) `compare` maximum (snd <$> d) of
        LT -> Just (-1)
        GT -> Just 1
        EQ -> Just $ compareDist s
  | (fst <$> Map.lookup (8, 3) s) == Just 1 = Just 1
  | (fst <$> Map.lookup (0, 3) s) == Just (-1) = Just (-1)
  | otherwise = Nothing

-- TODO Rat connot beat from pond  
moveS :: (State, Int) -> Coord -> Dir -> Maybe (State, Int)
moveS (s, k) c d = do
  (p, a) <- Map.lookup c s
  let c' = moveD c d
      jump :: Coord -> Dir -> Maybe Coord
      jump c d = do
        b <- is Pond c'
        if b
               -- A Tiger/Lion cannot jump over opponent's rat
          then if Just (other p, Rat) == Map.lookup c' s 
            then Nothing
            else jump c' d
          else return c'
        where c' = moveD c d
      move :: Animal -> Maybe Coord
      move Lion  = jump c d
      move Tiger = jump c d
      move Rat   = Just c'
      move a     = case is Pond c' of
        Just False -> Just c'
        _          -> Nothing
  c' <- move a
  guard (is (Cave p) c' == Just False)
  case Map.lookup c' s of
    Just (p', a')
        -- a rat connot beat from pond
      | is Pond c == Just True -> Nothing
      | p' == other p -> do
        t <- is Trap c'
        if t || beats a a'
          then Just (Map.insert c' (p, a) . Map.delete c $ s, 0)
          else Nothing
      | p' == p -> Nothing
    Nothing -> Just (Map.insert c' (p, a) . Map.delete c $ s, k + 1)

moves :: Player -> (State, Int) -> [(State, Int)]
moves p pair@(s, k) =
  let keys = Map.keys . Map.filter ((== p) . fst) $ s
  in do
    d <- dirs
    c <- keys
    maybeToList $ moveS pair c d

renderA :: (Player, Animal) -> Char
renderA (p, a) = (if p == 1 then toUpper else id) (case a of
  Rat -> 'r'
  Cat -> 'c'
  Dog -> 'd'
  Wolf -> 'w'
  Jaguar -> 'j'
  Tiger -> 't'
  Lion -> 'l'
  Elephant -> 'e')

renderT :: Tile -> Char
renderT t = case t of
  Field -> '.'
  Trap -> '#'
  Pond -> '~'
  Cave _ -> '*'

showS :: State -> String
showS s = unlines
  [ [ maybe (renderT $ tiles Map.! (i, j)) renderA (Map.lookup (i, j) s) 
      | j <- [0 .. 6]
    ]
    | i <- [0 .. 8]
  ]

initial :: State
initial = Map.fromList ((player 1 t) ++ (player (negate 1) $ mirror t))
  where
    t =
      [ ((0, 0), Lion)
      , ((0, 6), Tiger)
      , ((1, 1), Dog)
      , ((1, 5), Cat)
      , ((2, 0), Rat)
      , ((2, 2), Jaguar)
      , ((2, 4), Wolf)
      , ((2, 6), Elephant)
      ]
    player p = map (\(c, a) -> (c, (p, a)))
    mirror = map (\((i, j), x) -> ((8 - i, 6 - j), x))

tiles :: Map Coord Tile
tiles = 
  let ts = "..#^#..\n\
           \...#...\n\
           \.......\n\
           \.~~.~~.\n\
           \.~~.~~.\n\
           \.~~.~~.\n\
           \.......\n\
           \...#...\n\
           \..#*#..\n"
      decode '.' = Field
      decode '#' = Trap
      decode '^' = Cave 1
      decode '*' = Cave (-1)
      decode '~' = Pond
      cs = do
        (i, s) <- zip [0 ..] $ lines ts
        (j, c) <- zip [0 ..] s
        return ((i, j), decode c)
  in Map.fromList cs

is :: Tile -> Coord -> Maybe Bool
is x c = (x ==) <$> Map.lookup c tiles
