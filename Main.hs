--import Players
import Tile

main :: IO()
main = do
	sprite <- getSprite $ Tile Line (Treasure 5) East
	putStr $ unlines sprite

startup :: IO()
startup = do 
	putStrLn "Welcome to the labyrinth game."
	putStrLn "How many human players? (1-4)"
	humans <- getChar
	putStrLn $ [humans] ++ " human players."
