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
    <> drawDebug world
    <> translated  playerShift  0 (drawPlayer player state)
    <> translated offset 0 (drawGates (takeWhile (onScreen offset) gates))
    <> translated offset 0 (drawBoosts (takeWhile (onScreen offset) boosts))

  where
    maybeDrawMenu :: WorldState -> Picture
    maybeDrawMenu Progress = blank
    maybeDrawMenu Fail = lettering "Press space to try again" <> colored gray (solidRectangle 10 5)
    maybeDrawMenu Idle = lettering "Press space to start" <> colored gray (solidRectangle 10 5)

drawGate :: Gate -> Picture
drawGate (Gate width offsetX offsetY height) = top <> bottom
  where
    top = colored green (translated offsetX (offsetY + screenHeight / 2 + height / 2) (solidRectangle width screenHeight))
    bottom = colored green (translated offsetX (offsetY - screenHeight / 2 - height / 2) (solidRectangle width screenHeight))

drawBoost :: Boost -> Picture
drawBoost SlowMotion{color=color, radius=r, slowMotionOffsetX=x, slowMotionOffsetY=y} =
  colored color (translated x y (solidCircle r))

drawGates :: [Gate] -> Picture
drawGates = pictures . map drawGate

drawBoosts :: [Boost] -> Picture
drawBoosts = pictures . map drawBoost . filter (\SlowMotion{hidden = hidden} -> not hidden)

drawPlayer :: Player -> WorldState -> Picture
drawPlayer Player {y = y} state = translated 0 y maybeDeadPlayerPicture
  where
    playerPicture =lettering "\x1F6F8"
    maybeDeadPlayerPicture = case state of
      Fail -> reflected 0 playerPicture
      _ -> playerPicture

drawScore :: Int -> Picture
drawScore score = translated (-5) (-5) (lettering (T.pack (show score)))

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