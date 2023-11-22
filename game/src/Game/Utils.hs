module Game.Utils where

import Game.Data
import Game.Constants

onScreen :: (GameObject g) => Double -> g -> Bool
onScreen globalOffset g = (offsetX g + globalOffset) < screenWidth


offScreen :: (GameObject g) => Double -> g -> Bool
offScreen globalOffset g = (offsetX g + globalOffset) < -screenWidth
