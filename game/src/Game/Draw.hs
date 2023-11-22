module Game.Draw where

import Data.Text qualified as T
import CodeWorld
import Game.Data
import Game.Constants
import Game.Utils

drawWorld :: World -> Picture
drawWorld world@World {..} =
  maybeDrawMenu state
    <> drawScore score
    <> debugInfo
    <> translated  playerShift  0 (drawPlayer player immunity state)
    <> translated offset 0 (drawGameObjects offset gates)
    <> translated offset 0 (drawBoosts offset boosts)

  where
    debugInfo = if debugMode then drawDebug world else blank

    maybeDrawMenu :: WorldState -> Picture
    maybeDrawMenu Progress = blank
    maybeDrawMenu Fail = lettering "Press space to try again" <> colored gray (solidRectangle 10 5)
    maybeDrawMenu Idle = lettering "Press space to start" <> colored gray (solidRectangle 10 5)


drawGameObjects :: (GameObject g) => Double -> [g] -> Picture
drawGameObjects offset objects=  pictures $ map draw  onScreenObjects
  where
    onScreenObjects = takeWhile (onScreen offset) objects

drawBoosts :: (BoostObject b) => Double -> [b] -> Picture
drawBoosts offset b = drawGameObjects offset $ filter (not . hidden) b

drawPlayer :: Player -> Bool -> WorldState -> Picture
drawPlayer Player {y = y, hitBoxSize = playerSize} immunity state =
    translated 0 y (shield  <> maybeDeadPlayerPicture)
    where
        shield = if immunity then colored (translucent yellow) $ solidCircle playerSize else blank
        playerPicture = lettering "\x1F6F8"
        maybeDeadPlayerPicture = case state of
            Fail -> reflected 0 playerPicture
            _ -> playerPicture

drawScore :: Int -> Picture
drawScore score = translated (-screenWidth/2+ 1) (-screenHeight/2 + 1) (lettering (T.pack (show score)))

drawDebug :: World -> Picture
drawDebug World{speed=speed',
                currentSpeed=currentSpeed',
                activeBoosts=activeBoosts',
                boosts=boosts',
                offset=offset',
                player = Player{y=playerY, hitBoxSize=playerSize}} =
  colored red (rectangle screenWidth screenHeight)<>
  colored blue (solidCircle 0.1)<>
  translated 0 (-7) (lettering (T.pack (show speed'))) <>
  translated 0 (-8) (lettering (T.pack (show currentSpeed'))) <>
  translated (-3) (-9) (lettering (T.pack (show (length activeBoosts')))) <>
  translated 0 (-9) (lettering (T.pack (show (length $ takeWhile (onScreen offset') boosts'))))<>
  translated  playerShift  playerY (rectangle playerSize playerSize)