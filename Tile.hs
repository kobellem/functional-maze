module Tile (
	Tile(..),
	Kind(..),
	Direction(..),
	Treasure(..),
	drawTile
) where

import Numeric.Natural
import System.IO

data Kind = Corner | TShape | Line deriving (Show)
data Direction = North | East | South | West deriving (Show)
data Treasure = Treasure Natural

data Tile = Tile Kind Treasure Direction

drawTile :: Tile -> IO()
drawTile (Tile kind treasure direction) = 
	let path = "sprites/" ++ ((show kind) ++ (show direction)) ++ ".txt"
	in do
		contents <- readFile path
		putStr contents
