 module Board(
 	Board(..),
	drawBoard,
	insertTile
	) where

import Tile

import Data.List

data Board = Board [[Tile]] deriving (Show)

drawBoard :: Board -> IO()
drawBoard (Board tiles) =
	putStr $ unlines $ map concat $ concatMap transpose $ map (map getSprite) tiles

insertTile :: Board -> [Char] -> Int -> Tile -> (Board, Tile)
insertTile (Board tiles) dir n tile
	-- TODO make more generic
	| dir == "left" = 
		let newRow = (\row -> [tile] ++ (tail row)) $ tiles !! n
		in ((Board $ replaceNth n newRow tiles), (tiles !! n !! 6))
	| dir == "right" =
		let newRow = (\row -> (take 6 row) ++ [tile]) $ tiles !! n
		in ((Board $ replaceNth n newRow tiles), (head (tiles !! n)))
	| dir == "top" = ((Board $ moveColumnDown tiles n tile), (tiles !! 0 !! n))
	| dir == "bottom" = ((Board $ moveColumnUp tiles n tile), (tiles !! 6 !! n))

moveColumnDown :: [[Tile]] -> Int -> Tile -> [[Tile]]
moveColumnDown (t:ts) n tile
	| ts == [] = [replaceNth n tile t]
	| otherwise = [(replaceNth n tile t)] ++ (moveColumnDown ts n (t !! n))

moveColumnUp :: [[Tile]] -> Int -> Tile -> [[Tile]]
moveColumnUp tiles n tile = reverse $ moveColumnDown (reverse tiles) n tile
