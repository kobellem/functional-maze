 module Player (
	Player(..),
	playerColor,
	playerControl,
	playerPosition,
	playerCards
	) where

import Position
import Cards

data Color = Yellow | Red | Blue | Green
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
