module Players (
  Players(..)
) where

import Player

data Players = Players [Player]

-- current turn = first Player of the list
next :: Players -> Players
next (Players (x:xs)) = Players $ xs ++ [x]
