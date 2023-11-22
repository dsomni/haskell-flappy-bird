module Game.Data where

import System.Random ( StdGen )
import Game.Constants
import CodeWorld


data World = World
    { time :: Double,
      gates :: [Gate],
      boosts :: [Boost],
      activeBoosts:: [Boost],
      offset :: Double,
      speed :: Double,
      currentSpeed :: Double, -- with boosts etc
      player :: Player,
      score :: Int,
      state :: WorldState,
      generator :: StdGen,
      spacePressed :: Bool
    }


data WorldState = Progress | Fail | Idle


data Player = Player
    { velocity :: Double,
      y :: Double,
      hitBoxSize :: Double
    }


type Boost = SlowMotion

data Gate = Gate
    { gateWidth :: Double,
      gateOffsetX :: Double,
      gateOffsetY :: Double,
      gateHeight :: Double
    }
    deriving (Show)

data SlowMotion = SlowMotion
  {
    radius :: Double,
    slowMotionOffsetX :: Double,
    slowMotionOffsetY :: Double,
    slowMotionDuration :: Double,
    speedCoefficient:: Double,
    color :: Color,
    hidden :: Bool
  }
  deriving (Show)

class GameObject g where
    offsetX :: g -> Double
    offsetY :: g -> Double
    isCollided :: World -> g -> Bool



instance GameObject Gate where
    offsetX Gate{gateOffsetX=x} = x
    offsetY Gate{gateOffsetY=y} = y
    isCollided World {offset = worldOffset, player = Player {y = playerY, hitBoxSize = playerSize}}
        Gate {gateOffsetX = x, gateOffsetY = y, gateWidth = width, gateHeight = height}
        -- | True = False
        | abs playerY > screenHeight = True
        | ( (playerY + playerR) > (y + (height / 2) + collisionEpsilon)
            || (playerY - playerR) < (y - (height / 2) - collisionEpsilon)
        )
            && (-worldOffset + playerR) > (x - width / 2 + collisionEpsilon)
            && (-worldOffset - playerR) < (x + width / 2 - collisionEpsilon) =
            True
        | otherwise = False
        where
            playerR = playerSize / 2

instance GameObject SlowMotion where
    offsetX SlowMotion{slowMotionOffsetX=x} = x
    offsetY SlowMotion{slowMotionOffsetY=y} = y
    isCollided World {offset = worldOffset, player = Player {y = playerY, hitBoxSize = playerSize}}
        SlowMotion {slowMotionOffsetX = x, slowMotionOffsetY = y, radius = boostR, hidden=hidden}
        -- assume boost is square
        | hidden = False
        | ((playerY + playerR) > (y - boostR) && (playerY - playerR) < (y + boostR))
        &&((-worldOffset + playerR) > (x - boostR)  &&  (-worldOffset - playerR) < (x + boostR))
            = True
        | otherwise = False
        where
            playerR = playerSize / 2
