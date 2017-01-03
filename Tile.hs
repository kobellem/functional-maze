module Tile (
	Tile(..),
	Tiles(..),
	Kind(..),
	Direction(..),
	Treasure(..),
	getSprite
) where

import Numeric.Natural
import qualified Parser

data Kind = Corner | TShape | Line deriving (Eq, Show)
data Direction = North | East | South | West deriving (Eq, Show)
data Treasure = Treasure Natural deriving (Show)

data Tile = Tile Kind Treasure Direction
data Tiles = Tiles [Tile]

getTreasure :: Tile -> Treasure
getTreasure (Tile _ t _) = t

getSprite :: Tile -> [[Char]]
getSprite tile =
		let sprite = readSprite tile
		in (\line -> replaceNth 1 line sprite) (replaceNth 1 ((show (getTreasure tile)) !! 1) (sprite !! 1))

--deprecated, causes IO problems in further computations
--readSprite :: [Char] -> IO [[Char]]
--readSprite path = do
--	contents <- readFile path
--	return $ lines contents

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

replaceNth :: Int -> a -> [a] -> [a] 
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs
