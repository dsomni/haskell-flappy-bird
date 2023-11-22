module Game.Utils where

import Game.Constants
import Game.Data

onScreen :: (GameObject g) => Double -> g -> Bool
onScreen globalOffset g = (offsetX g + globalOffset) < screenWidth / 2

offScreen :: (GameObject g) => Double -> g -> Bool
offScreen globalOffset g = (offsetX g + globalOffset) < -screenWidth / 2
