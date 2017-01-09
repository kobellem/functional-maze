module Deserializer (
	tile
	) where

import Parser
import Tile

import qualified Control.Monad
import Numeric.Natural

kind :: Parser.Parser Kind
kind = Parser.fromRead

treasure :: Parser.Parser Treasure
treasure = Control.Monad.liftM Treasure Parser.fromRead

direction :: Parser.Parser Direction
direction = Parser.fromRead

tile :: Parser.Parser Tile
tile = Control.Monad.liftM3 Tile kind treasure direction
