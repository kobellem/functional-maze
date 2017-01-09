module Tile (
	Tile(..),
	Kind(..),
	Direction(..),
	Treasure(..),
	getSprite,
) where

import Control.Monad
import Numeric.Natural
import Text.Read

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

-- create the sprite
readSprite :: Tile -> [[Char]]
readSprite (Tile kind _ dir)
	| kind == Line && dir == North =
		["X X",
		 "X X",
		 "X X"]
	| kind == Line && dir == East =
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
