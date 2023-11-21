module Game.Draw where

import Data.Text qualified as T
import CodeWorld
import Game.Data
import Game.Constants
import Game.Utils

drawWorld :: World -> Picture
drawWorld World {..} =
  maybeDrawMenu state
    <> drawScore score
    <> translated
      playerShift
      0
      ( drawPlayer player state
      <> translated offset 0 (drawGates (takeWhile (onScreen offset) gates))
      <> translated offset 0 (drawBoosts (takeWhile (onScreen offset) boosts))
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
drawBoost SlowMotion{radius=r, slowMotionOffsetX=x, slowMotionOffsetY=y} =
  colored red (translated x y (solidCircle r))

drawGates :: [Gate] -> Picture
drawGates = pictures . map drawGate

drawBoosts :: [Boost] -> Picture
drawBoosts = pictures . map drawBoost . filter (\SlowMotion{hidden = hidden} -> not hidden)

drawPlayer :: Player -> WorldState -> Picture
drawPlayer Player {y = y, hitBoxSize = r} state = translated 0 y maybeDeadPlayerPicture
  where
    playerPicture = rectangle r r <> lettering "\x1F6F8"
    maybeDeadPlayerPicture = case state of
      Fail -> reflected 0 playerPicture
      _ -> playerPicture

drawScore :: Int -> Picture
drawScore score = translated (-5) (-5) (lettering (T.pack (show score)))