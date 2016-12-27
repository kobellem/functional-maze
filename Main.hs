-- import Players
import Tile

main :: IO()
main = do
	drawTile $ Tile Line (Treasure 5) North

startup :: IO()
startup = do 
	putStrLn "Welcome to the labyrinth game."
	putStrLn "How many human players? (1-4)"
	humans <- getChar
	putStrLn $ [humans] ++ " human players."
