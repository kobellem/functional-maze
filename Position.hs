module Position (
	Position(..)
	) where

data Position = Position Integer Integer

xPosition :: Position -> Integer
xPosition (Position x _) = x

yPosition :: Position -> Integer
yPosition (Position _ y) = y
