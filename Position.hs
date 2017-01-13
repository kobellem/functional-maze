module Position (
	Position(..),
	xPosition,
	yPosition,
	) where

import Numeric.Natural

data Position = Position Int Int deriving (Show)

xPosition :: Position -> Int
xPosition (Position x _) = x

yPosition :: Position -> Int
yPosition (Position _ y) = y
