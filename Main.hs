import Board
import Cards
--import Labyrinth
--import Player
import Players
import Position
import Tile
--import Tiles
import GameState


import Numeric.Natural
import System.Environment

main :: IO()
main = do
	--sprite <- getSprite $ Tile Line (Treasure 5) East
	--putStr $ unlines sprite
	args <- getArgs
	let nHumans = read (args !! 0) :: Int
	let nAIs = read (args !! 1) :: Int
	let nTotal = nHumans + nAIs
	putStr "starting game ..."
	if nTotal < 2 || nTotal > 4
	then putStr "Please decalare 2 to 4 players"
	else do
		game <- (newGame "./new.txt" nHumans nAIs)
		putStr $ show game 
