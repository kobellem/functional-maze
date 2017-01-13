 module Player (
	Player(..),
	Color(..),
	Control(..),
	getColor,
	getControl,
	getPosition,
	getCards,
	move
) where

import Board
import Position
import Cards
import Tile

data Color = Yellow | Red | Blue | Green deriving (Show)
data Control = Human | AI deriving (Show)

data Player = Player Color Control Position Cards deriving (Show)

getColor :: Player -> Color
getColor (Player color _ _ _) = color

getControl :: Player -> Control
getControl (Player _ control _ _) = control

getPosition :: Player -> Position
getPosition (Player _ _ pos _) = pos

getCards :: Player -> Cards
getCards (Player _ _ _ cards) = cards

move :: [Char] -> Board -> Player -> Maybe Player
move dir (Board tiles) (Player col cont pos cards)
	| dir == "left" =
		let newX = (xPosition pos) - 1
		in
			if newX < 0
			then Nothing
			else 
				if (hasConnection dir (tiles !! (yPosition pos) !! (xPosition pos))) && (hasConnection "right" (tiles !! (yPosition pos) !! newX))
				then Just $ Player col cont (Position newX $ yPosition pos) cards
				else Nothing
	| dir == "right" =
		let newX = (xPosition (pos)) + 1
		in
			if newX > 6
			then Nothing
			else 
				if (hasConnection dir (tiles !! (yPosition pos) !! (xPosition pos))) && (hasConnection "left" (tiles !! (yPosition pos) !! newX))
				then Just $ Player col cont (Position newX $ yPosition pos) cards
				else Nothing
	| dir == "up" =
		let newY = (yPosition (pos)) - 1
		in
			if newY < 0
			then Nothing
			else 
				if (hasConnection dir (tiles !! (yPosition pos) !! (xPosition pos))) && (hasConnection "down" (tiles !! newY !! (xPosition pos)))
				then Just $ Player col cont (Position (xPosition pos) newY) cards
				else Nothing
	| dir == "down" =
		let newY = (yPosition (pos)) + 1
		in
			if newY > 6
			then Nothing
			else 
				if (hasConnection dir (tiles !! (yPosition pos) !! (xPosition pos))) && (hasConnection "up" (tiles !! newY !! (xPosition pos)))
				then Just $ Player col cont (Position (xPosition pos) newY) cards
				else Nothing
