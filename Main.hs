import Cards
--import Labyrinth
import Player
import Players
import Position
import Tile
--import XTile

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
	else putStr $ show $ getCurrentPlayer $ next $ makePlayers (read (args !! 0) :: Int) (read (args !! 1) :: Int)

makePlayers :: Int -> Int -> Players
makePlayers nh na = Players $ map makePlayer $ (replicate nh "human") ++ (replicate na "ai")

makePlayer :: [Char] -> Player
makePlayer kind
	| kind == "human" = Player Red Human (Position 0 0) (Cards 0)
	| kind == "ai"		= Player Red AI (Position 0 0) (Cards 0)
