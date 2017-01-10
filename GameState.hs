module GameState (
	Game(..),
	getPlayers,
	newGame,
	draw
	) where

import Board
import Cards
import Parser
import Player
import Players
import Position
import Tile

import qualified Deserializer

import Control.Monad.Trans.State.Lazy
import Data.Tuple

data Game = Game Board [Tile] Players deriving (Show)

getPlayers :: Game -> Players
getPlayers (Game _ _ players) = players

newGame :: [Char] -> Int -> Int -> IO Game
newGame path humans ais =
	readFile path >>=
		\toParse ->	return $ (\(board, tiles) -> (Game board tiles (makePlayers humans ais))) (newBoard toParse)

newBoard :: [Char] -> (Board, [Tile])
newBoard s =
	-- TODO shuffle the tiles
	let (boardTiles, tiles) = createBoardTiles 49 (createTiles $ lines s)
	in ((Board $ splitAtEachN 7 boardTiles) ,tiles)

createTiles :: [String] -> [Tile]
createTiles ts = map fst $ map head $ map (Parser.apply Deserializer.tile) ts

createBoardTiles :: Int -> [Tile] -> ([Tile], [Tile])
createBoardTiles n tiles = ((take n tiles),(drop n tiles))

makePlayers :: Int -> Int -> Players
makePlayers nh na = 
	let players = map makePlayer $ (replicate nh "human") ++ (replicate na "ai")
	in Players $ zipWith (\ (Player _ control pos cards) color -> Player color control pos cards) players [Yellow, Red, Blue, Green] 

makePlayer :: [Char]  -> Player
makePlayer kind
	| kind == "human" = Player Red Human (Position 0 0) (Cards 0)
	| kind == "ai"		= Player Red AI (Position 0 0) (Cards 0)

draw :: Game -> IO()
draw (Game b _ _) = drawBoard b

splitAtEachN :: Int -> [a] -> [[a]]
splitAtEachN n lst =
	loop lst [[]] where
		loop :: [a] -> [[a]] -> [[a]]
		loop [] res = res
		loop l x = loop (drop n l) ([(take n l)] ++ x)
