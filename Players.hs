module Players (
  Players(..),
	getCurrentPlayer,
	next
) where

import Player

data Players = Players [Player] deriving (Show)

-- current turn = first Player of the list
getCurrentPlayer :: Players -> Player
getCurrentPlayer (Players lst) = head lst

next :: Players -> Players
next (Players (x:xs)) = Players $ xs ++ [x]
