module Game.Game where

import CodeWorld
import Data.List
import Data.Maybe
import Data.Text qualified as T
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

-- Position from where start building gates
startGates :: Double
startGates = 10.0

-- Spacing between neighbor gates
gatesSpacing :: Double
gatesSpacing = 6.0

sampleGates :: StdGen -> [Gate]
sampleGates gen =
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
    heights = randomRs (5.0, 10.0) g3
    ys = randomRs (-3.0, 3.0) g2
    xs = [startGates, startGates + gatesSpacing ..]

data WorldState = Progress | Fail | Idle

data World = World
  { time :: Double,
    gates :: [Gate],
    offset :: Double,
    speed :: Double,
    player :: Player,
    score :: Int,
    state :: WorldState,
    generator :: StdGen,
    spacePressed :: Bool
  }

drawGates :: [Gate] -> Picture
drawGates = pictures . map drawGate

drawPlayer :: Player -> WorldState -> Picture
drawPlayer Player {y = y', hitBoxSize = r} state = translated 0 y' playerPicture
  where
    playerPicture' = rectangle r r <> lettering "\x1F6F8"
    playerPicture = case state of
      Fail -> reflected 0 playerPicture'
      _ -> playerPicture'

drawScore :: Int -> Picture
drawScore score = translated (-5) (-5) (lettering (T.pack (show score)))

onScreen :: Double -> Gate -> Bool
onScreen globalOffset Gate {offsetX = offsetX'} = (offsetX' + globalOffset) < 50

playerShift :: Double
playerShift = -5

drawWorld :: World -> Picture
drawWorld World {gates = gates', offset = offset', player = player', score = score', state = state'} =
  maybeDrawMenu state'
    <> drawScore score'
    <> translated
      playerShift
      0
      ( drawPlayer player' state'
          <> translated offset' 0 (drawGates (takeWhile (onScreen offset') gates'))
      )
  where
    maybeDrawMenu :: WorldState -> Picture
    maybeDrawMenu Progress = blank
    maybeDrawMenu Fail = lettering "Press space to try again" <> colored gray (solidRectangle 10 5)
    maybeDrawMenu Idle = lettering "Press space to start" <> colored gray (solidRectangle 10 5)

drawGate :: Gate -> Picture
drawGate (Gate width offsetX offsetY height) = top <> bottom
  where
    windowHeight = 30
    top = colored green (translated offsetX (offsetY + windowHeight / 2 + height / 2) (solidRectangle width windowHeight))
    bottom = colored green (translated offsetX (offsetY - windowHeight / 2 - height / 2) (solidRectangle width windowHeight))

maxWorldSpeed :: Double
maxWorldSpeed = 3

worldSpeedIncrease :: Double
worldSpeedIncrease = 0.001

generateWorld :: StdGen -> World
generateWorld g = World (-2) (sampleGates gen) 0 1 (Player 0 0 1) 0 Idle gen False
  where
    (gen, _) = split g

newRandomGates :: IO ()
newRandomGates = do
  gen <- newStdGen
  activityOf (generateWorld gen) handleEvent drawWorld

handleProgressEvent :: Event -> World -> World
handleProgressEvent (TimePassing dt) world = updateWorld dt world
handleProgressEvent (KeyPress " ") world@World {player = player', spacePressed = False} = updateWorld 0 (world {player = player' {velocity = pushAcceleration}, spacePressed = True})
handleProgressEvent (KeyRelease " ") world = updateWorld 0 (world {spacePressed = False})
handleProgressEvent _ world = world

handleFailEvent :: Event -> World -> World
handleFailEvent (TimePassing dt) world = updateWorld dt world
handleFailEvent (KeyPress " ") World {generator = g} = generateWorld g
handleFailEvent _ world = world

handleEvent :: Event -> World -> World
handleEvent e world@World {state = Progress} = handleProgressEvent e world
handleEvent e world@World {state = Fail} = handleFailEvent e world
handleEvent e world@World {state = Idle} = handleIdleEvent e world

handleIdleEvent :: Event -> World -> World
handleIdleEvent (KeyPress " ") world = world {state = Progress}
handleIdleEvent _ world = world

offScreen :: Double -> Gate -> Bool
-- TODO: Check why here is '-6' but not '0'
offScreen globalOffset Gate {offsetX = offsetX'} = (offsetX' + globalOffset) < -6

updateWorld :: Double -> World -> World
updateWorld dt world@World {state = Fail, player = player'} = newStaticWorld
  where
    newFailedPlayer = updatePlayer dt player'
    newStaticWorld = world {speed = 0, player = newFailedPlayer}
updateWorld dt world@World {time = time', offset = offset', player = player', gates = gates', speed = speed', score = score'} =
  newWorld
  where
    newOffset = offset' - dt * speed'
    newWorld' = world {time = time' + dt, offset = newOffset, speed = max (speed' + worldSpeedIncrease) maxWorldSpeed}
    screenGates = takeWhile (onScreen offset') gates' -- TODO: consider only gates with offset <= player's position
    scoreImprovement = calculateScoreImprovement offset' newOffset screenGates
    newState = if isFailed newWorld' then Fail else Progress
    newPlayer' = case newState of
      Fail -> player' {velocity = pushAcceleration / 2}
      _ -> player'
    newPlayer = updatePlayer dt newPlayer'

    newWorld = newWorld' {state = newState, gates = dropWhile (offScreen offset') gates', score = score' + scoreImprovement, player = newPlayer}

calculateScoreImprovement :: Double -> Double -> [Gate] -> Int
calculateScoreImprovement oldOffset newOffset gates =
  length (filter (\Gate {offsetX = offset'} -> oldOffset + offset' > 0 && offset' + newOffset <= 0) gates)

isFailed :: World -> Bool
isFailed world@World {gates = gates', offset = offset'} = any (isCollided world) (takeWhile (onScreen offset') gates')

-- For better UX
collisionEpsilon :: Double
collisionEpsilon = 0.1

isCollided :: World -> Gate -> Bool
isCollided World {offset = worldOffset, player = Player {y = y', hitBoxSize = r}} Gate {offsetX = offsetX', offsetY = offsetY', width = width', height = height'}
  | abs y' > 10 = True -- TODO: create constant
  | ( (y' + rHalf) > (offsetY' + (height' / 2) + collisionEpsilon)
        || (y' - rHalf) < (offsetY' - (height' / 2) - collisionEpsilon)
    )
      && (-worldOffset + rHalf) > (offsetX' - width' / 2 + collisionEpsilon)
      && (-worldOffset - rHalf) < (offsetX' + width' / 2 - collisionEpsilon) =
      True
  | otherwise = False
  where
    rHalf = r / 2

updatePlayer :: Double -> Player -> Player
updatePlayer
  dt
  player@Player {velocity = velocity', y = y'} =
    player {velocity = newVelocity, y = newY}
    where
      newVelocity = velocity' + dt * gravity
      newY = max (y' + velocity' * dt) (-12)

data Player = Player
  { velocity :: Double,
    y :: Double,
    hitBoxSize :: Double
  }
