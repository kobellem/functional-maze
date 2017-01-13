import Game
import GameHandler

import Numeric.Natural
import System.Environment

main :: IO()
main = do
	--sprite <- getSprite $ Tile Line (Treasure 5) East
	--putStr $ unlines sprite
	args <- getArgs
	let 
		nHumans = read (args !! 0) :: Int
		nAIs = read (args !! 1) :: Int
		nTotal = nHumans + nAIs
	if nTotal < 2 || nTotal > 4
	then putStr "Please decalare 2 to 4 players"
	else do
		game <- newGame "saves/new.txt" nHumans nAIs
		start game
