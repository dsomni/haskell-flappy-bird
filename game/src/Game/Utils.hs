module Game.Utils where

import Game.Data

onScreen :: (GameObject g) => Double -> g -> Bool
onScreen globalOffset g = (offsetX g + globalOffset) < 50


offScreen :: (GameObject g) => Double -> g -> Bool
offScreen globalOffset g = (offsetX g + globalOffset) < -6
