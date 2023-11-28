module Game.Constants where

import CodeWorld

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

-- Acceleration after pressing <space>
smoothAcceleration :: Double
smoothAcceleration = (-gravity) * 1.5

-- Position from where start building obstacles
gatesShift :: Double
gatesShift = 2.0

playerShift :: Double
playerShift = -5

-- Spacing between neighbor gates
gatesSpacing :: Double
gatesSpacing = 6.0

maxWorldSpeed :: Double
maxWorldSpeed = 7

startWorldSpeed :: Double
startWorldSpeed = 3.5

worldSpeedIncrease :: Double
worldSpeedIncrease = 0.002

-- Some collisions tolerance or better UX
collisionEpsilon :: Double
collisionEpsilon = 0.05

-- How often player meets boosts
boostOccurrenceProbability :: Double
boostOccurrenceProbability = 0.7

-- How boost spawns inside gate (between tubes)
boostInsideGateProbability :: Double
boostInsideGateProbability = 0.5

-- How often spawn typeChanger gates
typeChangerGateProbability :: Double
typeChangerGateProbability = 0.1

-- How often spawn inverseGravity gates
inverseGravityGateProbability :: Double
inverseGravityGateProbability = 0.2

menuColor :: Color
menuColor = light gray

boostDefaultRadius :: Double
boostDefaultRadius = 0.5
