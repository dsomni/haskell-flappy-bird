module Game.Game where

import CodeWorld
import Data.List
import System.Random
import Game.Constants
import Game.Data
import Game.Draw
import Game.Utils

run :: IO ()
run = newGame



getSpawnFieldOutside :: Double ->Gate -> Gate -> (Double, Double, Double, Double)
getSpawnFieldOutside shift Gate {gateOffsetX = x1, gateWidth=w1}
  Gate {gateOffsetX = x2, gateWidth=w2} = (startX, endX, startY, endY)
    where
      startX = x1+w1/2 + shift
      endX = x2-w2/2 - shift
      startY = -(screenHeight/2) + shift
      endY = (screenHeight/2) - shift

getSpawnFieldInside :: Double ->Gate -> Gate -> (Double, Double, Double, Double)
getSpawnFieldInside shift Gate {gateOffsetX = x, gateOffsetY = y, gateWidth=w, gateHeight=h}
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
      SlowMotion
      (repeat radius)
      xs
      ys
      (repeat 10)
      (repeat 0.5)
      hidden


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
    xs = [gatesShift, gatesShift + gatesSpacing ..]

generateWorld :: WorldState -> StdGen -> World
generateWorld ws g = World (-2) gates boosts 0 1 (Player 0 0 1) 0 ws gen False
  where
    (gen, _) = split g
    gates = sampleGates gen
    boosts = sampleBoosts gen gates

newGame :: IO ()
newGame = do
  gen <- newStdGen
  activityOf (generateWorld Idle gen) handleEvent drawWorld

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
handleFailEvent (KeyPress " ") World {generator = g} = generateWorld Progress g
handleFailEvent _ world = world

handleIdleEvent :: Event -> World -> World
handleIdleEvent (KeyPress " ") world = world {state = Progress}
handleIdleEvent _ world = world



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
    boosts = dropWhile (offScreen offset) boosts, score = score + scoreImprovement, player = newPlayer}

calculateScoreImprovement :: Double -> Double -> [Gate] -> Int
calculateScoreImprovement oldOffset newOffset gates =
  length (filter (\Gate {gateOffsetX = offset'} -> oldOffset + offset' > 0 && offset' + newOffset <= 0) gates)

isFailed :: World -> Bool
isFailed world@World {gates = gates, offset = offset} = any (isCollided world) (takeWhile (onScreen offset) gates)



isCollided :: World -> Gate -> Bool
isCollided World {offset = worldOffset, player = Player {y = y, hitBoxSize = r}} Gate {gateOffsetX = offsetX, gateOffsetY = offsetY, gateWidth = width, gateHeight = height}
  | abs y > screenHeight = True
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

