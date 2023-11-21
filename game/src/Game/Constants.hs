module Game.Constants where

-- * System constants

screenWidth :: Double
screenWidth = 10

screenHeight :: Double
screenHeight = 10

-- * Game constants

gravity :: Double
gravity = -16

-- Instant acceleration after pressing <space>
pushAcceleration :: Double
pushAcceleration = 7

-- Position from where start building obstacles
gatesShift :: Double
gatesShift = 10.0

playerShift :: Double
playerShift = -5

-- Spacing between neighbor gates
gatesSpacing :: Double
gatesSpacing = 6.0


maxWorldSpeed :: Double
maxWorldSpeed = 3

worldSpeedIncrease :: Double
worldSpeedIncrease = 0.001

-- Some collisions tolerance or better UX
collisionEpsilon :: Double
collisionEpsilon = 0.1