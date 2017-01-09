 module Board(
 	Board(..),
	drawBoard
	) where

import Tile

import Data.List

data Board = Board [[Tile]] deriving (Show)

drawBoard :: Board -> IO()
drawBoard (Board tiles) =
	putStr $ unlines $ map concat $ concatMap transpose $ map (map getSprite) tiles
