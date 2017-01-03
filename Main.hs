import Board
import Cards
--import Labyrinth
import Player
import Players
import Position
import Tile
--import XTile

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
	if nTotal < 2 || nTotal > 4
	then putStr "Please decalare 2 to 4 players"
	else putStr $ (show $ getCurrentPlayer $ next $ makePlayers nHumans nAIs) ++ "\n"
	drawBoard $ Board [[(Tile Line (Treasure 1) East),(Tile TShape (Treasure 2) South)],[(Tile Corner (Treasure 3) South),(Tile Corner (Treasure 4) West)]]

makePlayers :: Int -> Int -> Players
makePlayers nh na = Players $ map makePlayer $ (replicate nh "human") ++ (replicate na "ai")

makePlayer :: [Char] -> Player
makePlayer kind
	| kind == "human" = Player Red Human (Position 0 0) (Cards 0)
	| kind == "ai"		= Player Red AI (Position 0 0) (Cards 0)
