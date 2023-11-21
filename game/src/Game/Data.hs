{-# LANGUAGE InstanceSigs #-}
module Game.Data where

import System.Random ( StdGen )


data World = World
    { time :: Double,
      gates :: [Gate],
      boosts :: [Boost],
      offset :: Double,
      speed :: Double,
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
    hidden :: Bool
  }
  deriving (Show)

class GameObject g where
    offsetX :: g -> Double
    offsetY :: g -> Double


instance GameObject Gate where
    offsetX Gate{gateOffsetX=x} = x
    offsetY Gate{gateOffsetY=y} = y

instance GameObject SlowMotion where
    offsetX SlowMotion{slowMotionOffsetX=x} = x
    offsetY SlowMotion{slowMotionOffsetY=y} = y