module Game.Constants where

-- * System constants

screenWidth :: Double
screenWidth = 20

screenHeight :: Double
screenHeight = 20

-- * Game constants

gravity :: Double
gravity = -16

-- Instant acceleration after pressing <space>
pushAcceleration :: Double
pushAcceleration = 7

-- Position from where start building obstacles
gatesShift :: Double
gatesShift = 2.0

playerShift :: Double
playerShift = -5

-- Spacing between neighbor gates
gatesSpacing :: Double
gatesSpacing = 6.0

maxWorldSpeed :: Double
maxWorldSpeed = 5

worldSpeedIncrease :: Double
worldSpeedIncrease = 0.003

-- Some collisions tolerance or better UX
collisionEpsilon :: Double
collisionEpsilon = 0.05

-- How often player meets boosts
boostOccurrenceProbability :: Double
boostOccurrenceProbability = 1

-- How boost spawns inside gate (between tubes)
boostInsideGateProbability :: Double
boostInsideGateProbability = 0.5
