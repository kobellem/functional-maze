module Position (
	Position(..),
	xPosition,
	yPosition
	) where

import Numeric.Natural

data Position = Position Natural Natural

xPosition :: Position -> Natural
xPosition (Position x _) = x

yPosition :: Position -> Natural
yPosition (Position _ y) = y
