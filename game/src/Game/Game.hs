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

      (randomValue,_) = randomR (0.0, 1.0) gen
      getPositions v
        | v < boostInsideGateProbability = getSpawnFieldInside
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
    radius' = 0.25
    generators = randomGens gen
    spawnFields = zipWith3 (getSpawnField radius') generators gates (drop 1 gates)
    (xs, ys) = unzip (zipWith generatePosition generators spawnFields)

    hidden' :: [Bool]
    hidden' = map ((>boostOccurrenceProbability) . fst . randomR (0,1)) generators

    boosts = zipWith6
      SlowMotion
      xs
      ys
      (repeat radius')
      (repeat 0.9)
      (repeat 5)
      hidden'


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
generateWorld ws g = World (-2) gates boosts [] 0 3 3 (Player 0 0 1) 0 ws gen False False
  where
    (gen, _) = split g
    gates = sampleGates gen
    boosts = sampleBoosts gen gates

newGame :: IO ()
newGame = do
  gen <- newStdGen
  activityOf (generateWorld Idle gen) handleEvent drawWorld

handleEvent :: Event -> World -> World
handleEvent (KeyPress "D") world@World{debugMode=oldDebug} = world {debugMode=not oldDebug}
handleEvent e world@World {state = Progress} = handleProgressEvent e world
handleEvent e world@World {state = Fail} = handleFailEvent e world
handleEvent e world@World {state = Idle} = handleIdleEvent e world

handleProgressEvent :: Event -> World -> World
handleProgressEvent (TimePassing dt) world = updateWorld dt world
handleProgressEvent (KeyPress " ") world@World {..}
  | spacePressed = updateWorld 0 world
  | otherwise =
      updateWorld 0 world
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



applyBoosts :: World -> World
applyBoosts world@World{activeBoosts=[]}  = world
applyBoosts world@World{activeBoosts=[b]}  = apply world b
applyBoosts world@World{activeBoosts=boost:boosts}  = apply (newWorld{activeBoosts=boost:boosts} ) boost
  where
    newWorld = applyBoosts world{activeBoosts=boosts}


addActiveBoosts :: World -> World
addActiveBoosts world@World{boosts=[]} = world
addActiveBoosts world@World{offset=globalOffset, boosts=(boost: restBoosts)}
  | onScreen globalOffset boost = newWorld {boosts=newBoost : newRestBoosts, activeBoosts=newActiveBoosts'}
  | otherwise = world
  where
    newWorld@World{boosts=newRestBoosts, activeBoosts=newActiveBoosts} = addActiveBoosts world {boosts=restBoosts}
    isNewBoostTaken = isCollided world boost
    newBoost = if isNewBoostTaken then  setHidden boost True else boost
    newActiveBoosts' = if isNewBoostTaken then newBoost:newActiveBoosts else newActiveBoosts



updateWorld :: Double -> World -> World
updateWorld dt world@World {state = Fail, player = player} = newStaticWorld
  where
    newFailedPlayer = updatePlayer dt player
    newStaticWorld = world {currentSpeed = 0, player = newFailedPlayer}
updateWorld dt world@World {..} =
  applyBoosts $ addActiveBoosts newWorldWithPlayer
  where
    newOffset = offset - dt * currentSpeed
    newSpeed = min (speed + worldSpeedIncrease) maxWorldSpeed
    newWorld = world {time = time + dt, offset = newOffset, speed = newSpeed}
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

    newActiveBoosts = filter (\b -> duration b >0) $ map (\b -> setDuration b (duration b-dt)) activeBoosts

    newWorldWithPlayer = newWorld
                            {state = newState,
                            currentSpeed=newSpeed,
                            gates = dropWhile (offScreen offset) gates,
                            boosts = dropWhile (offScreen offset) boosts,
                            activeBoosts = newActiveBoosts,
                            score = score + scoreImprovement,
                            player = newPlayer}


calculateScoreImprovement :: Double -> Double -> [Gate] -> Int
calculateScoreImprovement oldOffset newOffset gates =
  length (filter (\Gate {gateOffsetX = offset'} -> oldOffset + offset' > 0 && offset' + newOffset <= 0) gates)

isFailed :: World -> Bool
isFailed world@World {gates = gates, offset = offset} = any (isCollided world) (takeWhile (onScreen offset) gates)



updatePlayer :: Double -> Player -> Player
updatePlayer
  dt
  player@Player {velocity = velocity, y = y} =
    player {velocity = newVelocity, y = newY}
    where
      newVelocity = velocity + dt * gravity
      newY = max (y + velocity * dt) (-12)

