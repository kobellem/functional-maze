module Game (
	Game(..),
	newGame
	) where

import Board
import Cards
import Parser
import Player
import Players
import Position
import Tile

import qualified Deserializer

import Data.Tuple
import Debug.Trace

data Game = Game Board [Tile] Players deriving (Show)

-- create a new game
newGame :: [Char] -> Int -> Int -> IO Game
newGame path humans ais =
	readFile path >>=
			\toParse ->	newBoard toParse >>=
				\(board, tiles) -> return (Game board tiles (makePlayers humans ais))

newBoard :: [Char] -> IO (Board, [Tile])
newBoard str = do
	(boardTiles, tiles) <- createBoardTiles 49 (createTiles $ lines str)
	trace (show tiles) return ((Board $ splitAtEachN 7 boardTiles) ,tiles)

createTiles :: [String] -> [Tile]
createTiles ts = map fst $ map head $ map (Parser.apply Deserializer.tile) ts

createBoardTiles :: Int -> [Tile] -> IO ([Tile], [Tile])
createBoardTiles n tiles =
	loop (n - 1) ([], tiles) where
		loop :: Int -> ([Tile],[Tile]) -> IO ([Tile],[Tile])
		loop m res = do
			let (boardTiles, oldTiles) = res
			(tile, newTiles) <- takeRandom oldTiles
			newTile <- turnRandom tile
			if m == 0
			then return ((boardTiles ++ [newTile]), newTiles)
			else loop (m - 1) ((boardTiles ++ [newTile]), newTiles)
	
makePlayers :: Int -> Int -> Players
makePlayers nh na = 
	let players = map makePlayer $ (replicate nh "human") ++ (replicate na "ai")
	in Players $ zipWith3 (\ (Player _ control _ cards) color pos -> Player color control pos cards) players [Yellow, Red, Blue, Green] [(Position 1 1), (Position 1 6), (Position 6 1), (Position 6 6)]

makePlayer :: [Char]  -> Player
makePlayer kind
	| kind == "human" = Player Red Human (Position 0 0) (Cards 0)
	| kind == "ai"		= Player Red AI (Position 0 0) (Cards 0)

-- helpers
splitAtEachN :: Int -> [a] -> [[a]]
splitAtEachN n lst =
	loop lst [] where
		loop :: [a] -> [[a]] -> [[a]]
		loop [] res = res
		loop l x = loop (drop n l) ([(take n l)] ++ x)
