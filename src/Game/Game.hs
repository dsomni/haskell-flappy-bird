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
  }
  deriving (Show)

gravity :: Double
gravity = -16

pushAcceleration :: Double
pushAcceleration = 7

gateRemovalInterval :: Double
gateRemovalInterval = 3

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
    xs = [3.0, 6.0 ..]

data World = World
  { time :: Double,
    gates :: [Gate],
    offset :: Double,
    player :: Player,
    failed :: Bool
  }

drawGates :: [Gate] -> Picture
drawGates = pictures . map drawGate

drawPlayer :: Player -> Picture
drawPlayer Player {y = y'} = translated 0 y' (circle 0.5 <> lettering "\x1F6F8")

drawWorld :: World -> Picture
drawWorld World {gates = gates', offset = offset', player = player'} = translated (-5) 0 (drawPlayer player' <> translated offset' 0 (drawGates (take 10 gates')))

forceDrawGates :: [Gate] -> Picture
forceDrawGates gates = pictures (map drawGate gates)

drawGate :: Gate -> Picture
drawGate (Gate width offsetX offsetY height) = translated offsetX offsetY (colored black (rectangle width height))

newRandomGates :: IO ()
newRandomGates = do
  gen <- newStdGen
  let sampleWorld = World (-2) (sampleGates3 gen) 0 (Player 0 0 False) False
  activityOf sampleWorld handleEvent drawWorld

handleEvent :: Event -> World -> World
handleEvent (TimePassing dt) world@World {time = time', offset = offset'} = updateWorld dt (world {time = time' + dt, offset = offset' - dt})
handleEvent (KeyPress " ") world@World {player = player'@Player{pressedSpace=False}} = updateWorld 0 (world {player = player' {velocity = pushAcceleration, pressedSpace = True}})
handleEvent (KeyRelease " ") world@World {player = player'} = updateWorld 0 (world {player = player' {pressedSpace = False}})
handleEvent _ world = world

updateWorld :: Double -> World -> World
updateWorld _ world@World {failed = True} = world
updateWorld dt world@World {time = time', offset = offset', player = player', gates = gates'}
  | newTime > gateRemovalInterval = newWorld {time = time' - gateRemovalInterval, gates = drop 1 gates'}
  | otherwise = newWorld
  where
    newPlayer = updatePlayer dt player'
    newWorld'@World {time = newTime} = world {time = time' + dt, offset = offset' - dt, player = newPlayer}
    newWorld = newWorld' {failed = isFailed newWorld'}

isFailed :: World -> Bool
isFailed _ = False
-- isFailed world@World {gates = gates'} = any (isCollided world) (take 5 gates')

isCollided :: World -> Gate -> Bool
isCollided World {offset = worldOffset, player = Player {y = y'}} Gate {offsetX = offsetX', offsetY = offsetY', width = width', height = height'}
  | abs y' > 10 = True
  | ( y' >= (offsetY' + (height' / 2))
        || y' <= (offsetY' - (height' / 2))
    )
      && (-worldOffset) >= (offsetX' - width' / 2)
      && (-worldOffset) <= (offsetX' + width' / 2) =
      True
  | otherwise = False

updatePlayer :: Double -> Player -> Player
updatePlayer
  dt
  player@Player {velocity = velocity', y = y'} =
    player {velocity = newVelocity, y = newY}
    where
      newVelocity =  velocity' + dt * gravity
      newY = max (y' + velocity'*dt) (-10)

data Player = Player
  {
    velocity :: Double,
    y :: Double,
    pressedSpace :: Bool -- to prohibit player for holding SPACE
  }