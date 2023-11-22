module Game.Data where

import CodeWorld
import Game.Constants
import System.Random (StdGen)

data World = World
    { time :: Double
    , gates :: [Gate]
    , boosts :: [Boost]
    , activeBoosts :: [Boost]
    , offset :: Double
    , speed :: Double
    , currentSpeed :: Double -- with boosts etc
    , player :: Player
    , score :: Int
    , state :: WorldState
    , generator :: StdGen
    , spacePressed :: Bool
    , immunity :: Bool
    , debugMode :: Bool
    }

data WorldState = Progress | Fail | Idle

data Player = Player
    { velocity :: Double
    , y :: Double
    , hitBoxSize :: Double
    }

data Gate = Gate
    { gateWidth :: Double
    , gateOffsetX :: Double
    , gateOffsetY :: Double
    , gateHeight :: Double
    }
    deriving (Show)

data Boost
    = SlowMotion
        { boostOffsetX :: Double
        , boostOffsetY :: Double
        , boostRadius :: Double
        , speedCoefficient :: Double
        , boostDuration :: Double
        , boostHidden :: Bool
        }
    | Immunity
        { boostOffsetX :: Double
        , boostOffsetY :: Double
        , boostRadius :: Double
        , boostDuration :: Double
        , boostHidden :: Bool
        }

-- * Boost Object
class GameObject b => BoostObject b where
    radius :: b -> Double
    duration :: b -> Double
    hidden :: b -> Bool
    setRadius :: b -> Double -> b
    setDuration :: b -> Double -> b
    setHidden :: b -> Bool -> b
    color :: b -> Color
    apply :: World -> b -> World

    maximumDuration :: [b] -> Double
    maximumByDurationList :: [b] -> [b]

instance BoostObject Boost where
    radius = boostRadius
    duration = boostDuration
    hidden = boostHidden
    setRadius b newR = b{boostRadius = newR}
    setDuration b newD = b{boostDuration = newD}
    setHidden b newHidden = b{boostHidden = newHidden}
    color SlowMotion{} = red
    color Immunity{} = blue
    apply w@World{currentSpeed = speed'} SlowMotion{speedCoefficient = ratio} = w{currentSpeed = ratio * speed'}
    apply w Immunity{} = w{immunity = True}
    maximumDuration [] = -1
    maximumDuration [b] = duration b
    maximumDuration (b : bs) = max (duration b) (maximumDuration bs)
    maximumByDurationList bs = filter (\b -> maxDuration == duration b) bs
      where
        maxDuration = maximumDuration bs

-- * Game Object
class GameObject g where
    offsetX :: g -> Double
    offsetY :: g -> Double
    isCollided :: World -> g -> Bool
    draw :: g -> Picture

instance GameObject Gate where
    offsetX Gate{gateOffsetX = x} = x
    offsetY Gate{gateOffsetY = y} = y
    isCollided
        World
            { offset = worldOffset
            , immunity = immunity'
            , player = Player{y = playerY, hitBoxSize = playerSize}
            }
        Gate{gateOffsetX = x, gateOffsetY = y, gateWidth = width, gateHeight = height}
            | abs playerY > screenHeight / 2 = True -- like face with 'ground'
            | immunity' = False
            | ( (playerY + playerR) > (y + (height / 2) + collisionEpsilon)
                    || (playerY - playerR) < (y - (height / 2) - collisionEpsilon)
              )
                && (playerX + playerR) > (x - width / 2 + collisionEpsilon)
                && (playerX - playerR) < (x + width / 2 - collisionEpsilon) =
                True
            | otherwise = False
          where
            playerR = playerSize / 2
            playerX = -worldOffset + playerShift

    draw (Gate width offsetX' offsetY' height) = top <> bottom
      where
        top =
            colored
                green
                (translated offsetX' (offsetY' + screenHeight / 2 + height / 2) (solidRectangle width screenHeight))
        bottom =
            colored
                green
                (translated offsetX' (offsetY' - screenHeight / 2 - height / 2) (solidRectangle width screenHeight))

instance GameObject Boost where
    offsetX = boostOffsetX
    offsetY = boostOffsetY
    isCollided
        World{offset = worldOffset, player = Player{y = playerY, hitBoxSize = playerSize}}
        b
            -- assume boost is square
            | hidden' = False
            | ((playerY + playerR) > (y - boostR) && (playerY - playerR) < (y + boostR))
                && ((playerX + playerR) > (x - boostR) && (playerX - playerR) < (x + boostR)) =
                True
            | otherwise = False
          where
            boostR = radius b
            playerR = playerSize / 2
            playerX = -worldOffset + playerShift
            x = boostOffsetX b
            y = boostOffsetY b
            hidden' = boostHidden b
    draw b = colored (color b) (translated (offsetX b) (offsetY b) (solidCircle (radius b)))
