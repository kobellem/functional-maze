module Tile (
	Tile(..),
	Kind(..),
	Direction(..),
	Treasure(..),
	getSprite
) where

import Numeric.Natural

data Kind = Corner | TShape | Line deriving (Show)
data Direction = North | East | South | West deriving (Show)
data Treasure = Treasure Natural deriving (Show)

data Tile = Tile Kind Treasure Direction

getSprite :: Tile -> IO [[Char]]
getSprite (Tile kind treasure direction) =
	let s = readSprite $ "sprites/" ++ show kind ++ show direction ++ ".txt"
	in do
		sprite <- s
		return $ (\line -> replaceNth 2 line sprite) (replaceNth 2 ((show treasure) !! 1) (sprite !! 2)) 

readSprite :: [Char] -> IO [[Char]]
readSprite path = do
	contents <- readFile path
	return $ lines contents

replaceNth :: Int -> a -> [a] -> [a] 
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs
