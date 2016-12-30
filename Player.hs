 module Player (
	Player(..),
	Color(..),
	Control(..),
	playerColor,
	playerControl,
	playerPosition,
	playerCards
) where

import Position
import Cards

data Color = Yellow | Red | Blue | Green deriving (Enum)
data Control = Human | AI

data Player = Player Color Control Position Cards

playerColor :: Player -> Color
playerColor (Player color _ _ _) = color

playerControl :: Player -> Control
playerControl (Player _ control _ _) = control

playerPosition :: Player -> Position
playerPosition (Player _ _ pos _) = pos

playerCards :: Player -> Cards
playerCards (Player _ _ _ cards) = cards

-- Show instance declarations
instance Show Color where
	show Yellow = "Yellow"
	show Red = "Red"
	show Blue = "Blue"
	show Green = "Green"

instance Show Control where
	show Human = "Human"
	show AI = "AI"

instance Show Player where
	show player = (show (playerControl player)) ++ " Player " ++ (show (playerColor player))
