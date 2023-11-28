{-# LANGUAGE InstanceSigs #-}

module Game.Data where

import CodeWorld
import Game.Constants
import System.Random (StdGen)

data GameType = Pushing | Holding

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
    , gameType :: GameType
    , inverseGravity :: Bool
    }

data WorldState = Progress | Fail | Idle

data Player = Player
    { velocity :: Double
    , y :: Double
    , hitBoxSize :: Double
    , acceleration :: Double
    }

data GateType = OrdinaryGate | GameTypeChangerGate | GravityInverseGate
data Gate = Gate
    { gateWidth :: Double
    , gateOffsetX :: Double
    , gateOffsetY :: Double
    , gateHeight :: Double
    , gateType :: GateType
    }

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
    | Forcing
        { boostOffsetX :: Double
        , boostOffsetY :: Double
        , boostRadius :: Double
        , boostDuration :: Double
        , boostHidden :: Bool
        , forcingDuration :: Double
        , forcingSpeed :: Double
        }

-- * Boost Object

class (GameObject b) => BoostObject b where
    radius :: b -> Double
    duration :: b -> Double
    hidden :: b -> Bool
    setRadius :: b -> Double -> b
    decreaseDuration :: b -> Double -> b
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

    decreaseDuration b@Forcing{boostDuration = boostD, forcingDuration = forcingD} d' =
        b
            { boostDuration = boostD - d'
            , forcingDuration = forcingD - d'
            }
    decreaseDuration b d' = b{boostDuration = duration b - d'}
    setHidden :: Boost -> Bool -> Boost
    setHidden b newHidden = b{boostHidden = newHidden}
    color SlowMotion{} = red
    color Immunity{} = blue
    color Forcing{} = purple
    apply w@World{currentSpeed = speed'} SlowMotion{speedCoefficient = ratio} = w{currentSpeed = ratio * speed'}
    apply w Immunity{} = w{immunity = True}
    apply w@World{currentSpeed = s} Forcing{forcingDuration = d, forcingSpeed = s'} = w{immunity = True, currentSpeed = newSpeed}
      where
        newSpeed = if d > 0 then s' else s
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

    draw (Gate width offsetX' offsetY' height gateType') = translated offsetX' 0 (top <> middle <> bottom)
      where
        top =
            colored
                green
                (translated 0 (offsetY' + screenHeight / 2 + height / 2) (solidRectangle width screenHeight))
        bottom =
            colored
                green
                (translated 0 (offsetY' - screenHeight / 2 - height / 2) (solidRectangle width screenHeight))

        middle = case gateType' of
            OrdinaryGate -> blank
            GameTypeChangerGate -> colored (translucent pink) (translated 0 offsetY' (solidRectangle width height))
            GravityInverseGate -> colored (translucent orange) (translated 0 offsetY' (solidRectangle width height))

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
    draw b@Forcing{} = colored (color b) (translated (offsetX b) (offsetY b) (lettering "\x1F680"))
    draw b@SlowMotion{} = colored (color b) (translated (offsetX b) (offsetY b) (lettering "\x1F40C"))
    draw b@Immunity{} = colored (color b) (translated (offsetX b) (offsetY b) (lettering "\x1F6E1"))
