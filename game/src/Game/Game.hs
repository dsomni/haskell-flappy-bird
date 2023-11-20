module Game.Game where

import CodeWorld
import Data.List
import Data.Text qualified as T
import System.Random

run :: IO ()
run = newGame

data Gate = Gate
  { width :: Double,
    offsetX :: Double,
    offsetY :: Double,
    height :: Double
  }
  deriving (Show)


data SlowMotionBoost= SlowMotionBoost
  {
    radius :: Double,
    offsetX_ :: Double,
    offsetY_ :: Double,
    duration :: Double,
    speedCoefficient:: Double,
    hidden :: Bool
  }
  deriving (Show)

type Boost = SlowMotionBoost


screenWidth :: Double
screenWidth = 10

screenHeight :: Double
screenHeight = 10


getSpawnFieldOutside :: Double ->Gate -> Gate -> (Double, Double, Double, Double)
getSpawnFieldOutside shift Gate {offsetX = x1, width=w1}
  Gate {offsetX = x2, width=w2} = (startX, endX, startY, endY)
    where
      startX = x1+w1/2 + shift
      endX = x2-w2/2 - shift
      startY = -(screenHeight/2) + shift
      endY = (screenHeight/2) - shift

getSpawnFieldInside :: Double ->Gate -> Gate -> (Double, Double, Double, Double)
getSpawnFieldInside shift Gate {offsetX = x, offsetY = y, width=w, height=h}
  _ = (startX, endX, startY, endY)
    where
      startX = x-w/2 + shift
      endX = x+w/2 - shift
      startY = y-h/2 + shift
      endY = y+h/2  - shift

getSpawnField :: Double -> StdGen->  Gate -> Gate -> (Double, Double, Double, Double)
getSpawnField shift gen = getPositions randomValue shift
    where
      insideProbability :: Double
      insideProbability = 0.3
      (randomValue,_) = randomR (0.0, 1.0) gen
      getPositions v
        | v < insideProbability = getSpawnFieldInside
        | otherwise = getSpawnFieldOutside


generatePosition :: StdGen->(Double, Double, Double, Double) -> (Double,Double)
generatePosition gen (startX, endX, startY, endY) = (x, y)
  where
    (x, _) = randomR (startX, endX) gen
    (y, _) = randomR (startY, endY) gen


randomGens :: StdGen -> [StdGen]
randomGens = unfoldr (Just . split)

sampleBoosts :: StdGen -> [Gate] -> [Boost]
sampleBoosts gen gates = boosts
  where
    radius = 0.25
    generators = randomGens gen
    spawnFields = zipWith3 (getSpawnField radius) generators gates (drop 1 gates)
    (xs, ys) = unzip (zipWith generatePosition generators spawnFields)

    occurrenceProbability :: Double
    occurrenceProbability = 0.4

    hidden :: [Bool]
    hidden = map ((>occurrenceProbability) . fst . randomR (0,1)) generators

    boosts = zipWith6
      SlowMotionBoost
      (repeat radius)
      xs
      ys
      (repeat 10)
      (repeat 0.5)
      hidden


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
    boosts :: [Boost],
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

drawBoosts :: [Boost] -> Picture
drawBoosts = pictures . map drawBoost . filter (\SlowMotionBoost{hidden = hidden} -> not hidden)

drawPlayer :: Player -> WorldState -> Picture
drawPlayer Player {y = y, hitBoxSize = r} state = translated 0 y maybeDeadPlayerPicture
  where
    playerPicture = rectangle r r <> lettering "\x1F6F8"
    maybeDeadPlayerPicture = case state of
      Fail -> reflected 0 playerPicture
      _ -> playerPicture

drawScore :: Int -> Picture
drawScore score = translated (-5) (-5) (lettering (T.pack (show score)))

onScreen :: Double -> Gate -> Bool
onScreen globalOffset Gate {offsetX = offsetX} = (offsetX + globalOffset) < 50

onScreenBoost :: Double -> Boost -> Bool
onScreenBoost globalOffset SlowMotionBoost{offsetX_ = offsetX} = (offsetX + globalOffset) < 50

playerShift :: Double
playerShift = -5

drawWorld :: World -> Picture
drawWorld World {..} =
  maybeDrawMenu state
    <> drawScore score
    <> translated
      playerShift
      0
      ( drawPlayer player state
      <> translated offset 0 (drawGates (takeWhile (onScreen offset) gates))
      <> translated offset 0 (drawBoosts (takeWhile (onScreenBoost offset) boosts))
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

drawBoost :: Boost -> Picture
drawBoost SlowMotionBoost{radius=r,offsetX_=x, offsetY_=y} =
  colored red (translated x y (solidCircle r))

maxWorldSpeed :: Double
maxWorldSpeed = 3

worldSpeedIncrease :: Double
worldSpeedIncrease = 0.001

generateWorld :: StdGen -> World
generateWorld g = World (-2) gates boosts 0 1 (Player 0 0 1) 0 Idle gen False
  where
    (gen, _) = split g
    gates = sampleGates gen
    boosts = sampleBoosts gen gates

newGame :: IO ()
newGame = do
  gen <- newStdGen
  activityOf (generateWorld gen) handleEvent drawWorld

handleEvent :: Event -> World -> World
handleEvent e world@World {state = Progress} = handleProgressEvent e world
handleEvent e world@World {state = Fail} = handleFailEvent e world
handleEvent e world@World {state = Idle} = handleIdleEvent e world

handleProgressEvent :: Event -> World -> World
handleProgressEvent (TimePassing dt) world = updateWorld dt world
handleProgressEvent (KeyPress " ") world@World {..}
  | spacePressed = world
  | otherwise =
      updateWorld 0 $
        world
          { player = player {velocity = pushAcceleration},
            spacePressed = True
          }
handleProgressEvent (KeyRelease " ") world = updateWorld 0 (world {spacePressed = False})
handleProgressEvent _ world = world

handleFailEvent :: Event -> World -> World
handleFailEvent (TimePassing dt) world = updateWorld dt world
handleFailEvent (KeyPress " ") World {generator = g} = generateWorld g
handleFailEvent _ world = world

handleIdleEvent :: Event -> World -> World
handleIdleEvent (KeyPress " ") world = world {state = Progress}
handleIdleEvent _ world = world

offScreen :: Double -> Gate -> Bool
offScreen globalOffset Gate {offsetX = offsetX} = (offsetX + globalOffset) < -6

offScreenBoost :: Double -> Boost -> Bool
offScreenBoost globalOffset SlowMotionBoost {offsetX_ = offsetX} = (offsetX + globalOffset) < -6

updateWorld :: Double -> World -> World
updateWorld dt world@World {state = Fail, player = player} = newStaticWorld
  where
    newFailedPlayer = updatePlayer dt player
    newStaticWorld = world {speed = 0, player = newFailedPlayer}
updateWorld dt world@World {..} =
  newWorldWithPlayer
  where
    newOffset = offset - dt * speed
    newWorld = world {time = time + dt, offset = newOffset, speed = max (speed + worldSpeedIncrease) maxWorldSpeed}
    screenGates = takeWhile (onScreen offset) gates -- TODO: consider only gates with offset <= player's position
    scoreImprovement = calculateScoreImprovement offset newOffset screenGates
    newState = if isFailed newWorld then Fail else Progress
    newPlayer =
      updatePlayer
        dt
        ( case newState of
            Fail -> player {velocity = pushAcceleration / 2}
            _ -> player
        )

    newWorldWithPlayer = newWorld {state = newState, gates = dropWhile (offScreen offset) gates,
    boosts = dropWhile (offScreenBoost offset) boosts, score = score + scoreImprovement, player = newPlayer}

calculateScoreImprovement :: Double -> Double -> [Gate] -> Int
calculateScoreImprovement oldOffset newOffset gates =
  length (filter (\Gate {offsetX = offset'} -> oldOffset + offset' > 0 && offset' + newOffset <= 0) gates)

isFailed :: World -> Bool
isFailed world@World {gates = gates, offset = offset} = any (isCollided world) (takeWhile (onScreen offset) gates)

-- For better UX
collisionEpsilon :: Double
collisionEpsilon = 0.1

isCollided :: World -> Gate -> Bool
isCollided World {offset = worldOffset, player = Player {y = y, hitBoxSize = r}} Gate {offsetX = offsetX, offsetY = offsetY, width = width, height = height}
  | abs y > 10 = True -- TODO: create constant
  | ( (y + rHalf) > (offsetY + (height / 2) + collisionEpsilon)
        || (y - rHalf) < (offsetY - (height / 2) - collisionEpsilon)
    )
      && (-worldOffset + rHalf) > (offsetX - width / 2 + collisionEpsilon)
      && (-worldOffset - rHalf) < (offsetX + width / 2 - collisionEpsilon) =
      True
  | otherwise = False
  where
    rHalf = r / 2

updatePlayer :: Double -> Player -> Player
updatePlayer
  dt
  player@Player {velocity = velocity, y = y} =
    player {velocity = newVelocity, y = newY}
    where
      newVelocity = velocity + dt * gravity
      newY = max (y + velocity * dt) (-12)

data Player = Player
  { velocity :: Double,
    y :: Double,
    hitBoxSize :: Double
  }
