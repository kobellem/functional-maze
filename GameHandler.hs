module GameHandler (
	start
	) where

import Board
import Game
import Player
import Players
import Tile

import Data.Maybe
import qualified System.Process
import Text.Read

import Debug.Trace

start :: Game -> IO()
start game = do 
	putStrLn("Welcome to the labyrinth game.")
	_ <- newScreen
	nextTurn game

nextTurn :: Game -> IO()
nextTurn (Game board tiles players) = do
	let currentPlayer = getCurrentPlayer players
	putStr $ (show $ getColor currentPlayer) ++ " players turn\n"
	_ <- newScreen
	putStr $ (show $ getColor currentPlayer) ++ "\n\n"
	drawBoard board
	putStr $ show players
	putStr $ "\nYour Cards: " ++ (show $ getCards currentPlayer)
	putStr $ "\nYour Tile:\n" ++ ((show $ head tiles) ++ "\n")
	(newBoard, rtile) <- insertCommand board $ head tiles
	_ <- newScreen
	drawBoard newBoard
	putStr $ show players
	putStr "Move:\n"
	newPlayers <- movePlayer players board
	_ <- newScreen
	nextTurn $ Game newBoard (tiles ++ [rtile]) $ next newPlayers 

insertCommand :: Board -> Tile -> IO (Board, Tile)
insertCommand board tile = do
	command <- getLine
	let res = parseInsertCommand board tile command
	if isJust res
	then return $ fromJust res
	else do
		putStr "Wrong input format, please try again.\n"
		insertCommand board tile

-- TODO move players in affected row/column
parseInsertCommand :: Board -> Tile -> [Char] -> Maybe (Board, Tile)
parseInsertCommand board (Tile kind treasure _) s = do
	let command = lines $ map (\c -> if c == ' ' then '\n' else c) s
	if not $ null command
	then 
		if elem (command !! 0) ["left", "right", "top", "bottom"]
		then
			if elem (read (command !! 1) :: Int) [1..7]
			then
				let dir = readMaybe (command !! 2) :: Maybe Direction
				in
					if isJust dir
					then Just $ insertTile board (command !! 0) ((read (command !! 1) :: Int) - 1) $ Tile kind treasure $ fromJust dir
					else Nothing
			else Nothing
		else Nothing
	else Nothing

movePlayer :: Players -> Board -> IO Players
movePlayer (Players players) board = do
 command <- getLine
 if elem (command) ["left", "right", "up", "down"]
 then 
 	let player = move command board $ head players
 	in
		if trace (show player) isJust player
		then return $ Players $ [fromJust player] ++ (tail players)
		else do
			putStr "Illegal move"
			movePlayer (Players players) board
 else do
 	putStr "Wrong input"
	movePlayer (Players players) board

newScreen :: IO String
newScreen = do
	out <- getLine
	System.Process.callCommand "clear"
	return out
