module Game.Game where

import CodeWorld
import Data.List
import System.Random

run :: IO ()
run = newRandomGates

data Gate = Gate
  { width :: Double,
    offsetX :: Double,
    offsetY :: Double,
    height :: Double
  }  deriving (Show)

gravity :: Double
gravity = -16

pushAcceleration :: Double
pushAcceleration = 7

-- Position from where start building gates
startGates :: Double
startGates = 10.0

-- Spacing between neighbor gates
gatesSpacing:: Double
gatesSpacing = 5.0

sampleGates3 :: StdGen -> [Gate]
sampleGates3 gen =
  zipWith4
    Gate
    widths
    xs
    ys
    heights
  where
    (g1', g2) = split gen
    (g1, g3) = split g1'
    widths = randomRs (2.0, 3.0) g1
    heights = randomRs (3.0, 10.0) g3
    ys = randomRs (-3.0, 3.0) g2
    xs = [startGates, startGates+gatesSpacing ..]

data World = World
  { time :: Double,
    gates :: [Gate],
    offset :: Double,
    speed :: Double,
    player :: Player,
    failed :: Bool
  }

drawGates :: [Gate] -> Picture
drawGates = pictures . map drawGate

drawPlayer :: Player -> Picture
drawPlayer Player {y = y', hitBoxSize= r} = translated 0 y' (rectangle r r <> lettering "\x1F6F8")


onScreen :: Double -> Gate -> Bool
onScreen globalOffset Gate {offsetX = offsetX'} = (offsetX' + globalOffset) < 50

drawWorld :: World -> Picture
drawWorld World {gates = gates', offset = offset', player = player'} = translated (-5) 0 (drawPlayer player' <> translated offset' 0 (drawGates (takeWhile (onScreen offset') gates')))

drawGate :: Gate -> Picture
drawGate (Gate width offsetX offsetY height) = top <> bottom
  where
    windowHeight = 30
    top = colored green (translated offsetX (offsetY + windowHeight/2 + height/2) (solidRectangle width windowHeight))
    bottom = colored green (translated offsetX (offsetY-windowHeight/2-height/2) (solidRectangle width windowHeight))


newRandomGates :: IO ()
newRandomGates = do
  gen <- newStdGen
  let sampleWorld = World (-2) (sampleGates3 gen) 0 1 (Player 0 0 False 1) False
  activityOf sampleWorld handleEvent drawWorld

handleEvent :: Event -> World -> World
handleEvent (TimePassing dt) world@World {time = time', offset = offset'} = updateWorld dt (world {time = time' + dt, offset = offset' - dt})
handleEvent (KeyPress " ") world@World {player = player'@Player{pressedSpace=False}} = updateWorld 0 (world {player = player' {velocity = pushAcceleration, pressedSpace = True}})
handleEvent (KeyRelease " ") world@World {player = player'} = updateWorld 0 (world {player = player' {pressedSpace = False}})
handleEvent _ world = world

offScreen :: Double -> Gate -> Bool
-- TODO: Check why here is '-6' but not '0'
offScreen globalOffset Gate {offsetX = offsetX'} = (offsetX' + globalOffset) < -6

updateWorld :: Double -> World -> World
updateWorld _ world@World {failed = True} = world
updateWorld dt world@World {time = time', offset = offset', player = player', gates = gates'}
  = newWorld
  where
    newPlayer = updatePlayer dt player'
    newWorld'@World {speed=speed'} = world {time = time' + dt, offset = offset' - dt*speed', player = newPlayer}
    newWorld = newWorld' {failed = isFailed newWorld', gates = dropWhile (offScreen offset') gates'}

isFailed :: World -> Bool
isFailed world@World {gates = gates', offset=offset'} = any (isCollided world) (takeWhile (onScreen offset') gates')


-- For better UX
collisionEpsilon :: Double
collisionEpsilon = 0.1

isCollided :: World -> Gate -> Bool
isCollided World {offset = worldOffset, player = Player {y = y', hitBoxSize=r}} Gate {offsetX = offsetX', offsetY = offsetY', width = width', height = height'}
  | abs y' > 10 = True -- TODO: create constant
  | ( (y'+r) > ( offsetY' + (height' / 2) + collisionEpsilon)
        || (y'-r) < (offsetY' - (height' / 2)- collisionEpsilon)
    )
      && (-worldOffset +r) > (offsetX' - width' / 2 + collisionEpsilon)
      && (-worldOffset-r) < (offsetX' + width' / 2 - collisionEpsilon) =
      True
  | otherwise = False

updatePlayer :: Double -> Player -> Player
updatePlayer
  dt
  player@Player {velocity = velocity', y = y'} =
    player {velocity = newVelocity, y = newY}
    where
      newVelocity =  velocity' + dt * gravity
      newY = max (y' + velocity'*dt) (-12)

data Player = Player
  {
    velocity :: Double,
    y :: Double,
    pressedSpace :: Bool, -- to prohibit player for holding SPACE
    hitBoxSize :: Double
  }
