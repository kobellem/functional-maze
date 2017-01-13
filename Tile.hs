module Tile (
	Tile(..),
	Kind(..),
	Direction(..),
	Treasure(..),
	getSprite,
	takeRandom,
	turnRandom,
	hasConnection,
	replaceNth
) where

import System.Random

import Numeric.Natural

import Debug.Trace

data Kind = Corner | TShape | Line deriving (Eq, Show, Read)
data Direction = North | East | South | West deriving (Eq, Show, Read)
data Treasure = Treasure Natural deriving (Eq, Show, Read)

data Tile = Tile Kind Treasure Direction deriving (Eq, Show, Read)

-- getters
getKind :: Tile -> Kind
getKind (Tile k _ _) = k

getTreasure :: Tile -> Treasure
getTreasure (Tile _ t _) = t

getDirection :: Tile -> Direction
getDirection (Tile _ _ d) = d

getSprite :: Tile -> [[Char]]
getSprite tile =
		let sprite = readSprite tile
		in (\line -> replaceNth 1 line sprite) (replaceNth 1 ((show (getTreasure tile)) !! 1) (sprite !! 1))

hasConnection :: [Char] -> Tile -> Bool
hasConnection dir tile
	| dir == "left" = ' ' == ((readSprite tile) !! 1 !! 0)
	| dir == "right" = ' ' == ((readSprite tile) !! 1 !! 2)
	| dir == "up" = ' ' == ((readSprite tile) !! 0 !! 1)
	| dir == "down" = ' ' == ((readSprite tile) !! 2 !! 1)

--take a random tile from a list of tiles
takeRandom :: [Tile] -> IO (Tile, [Tile])
takeRandom tiles =
	newStdGen >>=
		\gen -> return $ takeNth ((fst (random gen)) `mod` (length tiles)) tiles

turnRandom :: Tile -> IO Tile
turnRandom (Tile k t _) =
	newStdGen >>=
		\gen -> ((\dir -> return $ Tile k t dir) ([North, East, South, West] !! ((fst (random gen)) `mod` 4)))

-- create the sprite
readSprite :: Tile -> [[Char]]
readSprite (Tile kind _ dir)
	| kind == Line && (dir == North || dir == South) =
		["X X",
		 "X X",
		 "X X"]
	| kind == Line && (dir == East || dir == West) =
		["XXX",
		 "   ",
		 "XXX"]
	| kind == TShape && dir == North =
		["X X",
		 "   ",
		 "XXX"]
	| kind == TShape && dir == East =
		["X X",
		 "X  ",
		 "X X"]
	| kind == TShape && dir == South =
		["XXX",
		 "   ",
		 "X X"]
	| kind == TShape && dir == West =
		["X X",
		 "  X",
		 "X X"]
	| kind == Corner && dir == North =
		["X X",
		 "X  ",
		 "XXX"]
	| kind == Corner && dir == East =
		["XXX",
		 "X  ",
		 "X X"]
	| kind == Corner && dir == South =
		["XXX",
		 "  X",
		 "X X"]
	| kind == Corner && dir == West =
		["X X",
		 "  X",
		 "XXX"]

-- help functions
replaceNth :: Int -> a -> [a] -> [a] 
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

takeNth :: Int -> [a] -> (a, [a])
takeNth n tiles = ((tiles !! n), (let (x,y) = splitAt n tiles in x ++ (tail y)))
