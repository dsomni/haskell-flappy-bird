{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Draw where

import CodeWorld
import qualified Data.Text as T
import Game.Constants
import Game.Data
import Game.Utils

drawWorld :: World -> Picture
drawWorld world@World{..} =
    maybeDrawMenu state score
        <> drawScore score
        <> drawGameType gameType
        <> drawActiveBoosts activeBoosts
        <> debugInfo
        <> translated playerShift 0 (drawPlayer player immunity inverseGravity state)
        <> translated offset 0 (drawGameObjects offset gates)
        <> translated offset 0 (drawBoosts offset boosts)
  where
    debugInfo = if debugMode then drawDebug world else blank

    maybeDrawMenu :: WorldState -> Int -> Picture
    maybeDrawMenu Progress _ = blank
    maybeDrawMenu Fail score' = translated 0 1 (colored red $ lettering "GAME OVER") <> lettering ("Your score: " <> T.pack (show score')) <> translated 0 (-1.2) (lettering "Press space to try again") <> colored menuColor (solidRectangle 10 5)
    maybeDrawMenu Idle _ = lettering "Press space to start" <> colored menuColor (solidRectangle 10 5)

roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (truncate $ x * 10 ^ n) / 10 ^ n

drawGameType :: GameType -> Picture
drawGameType gameType = translated 0 (screenHeight / 2 - 1) (lettering text)
  where
    text = case gameType of
        Pushing -> "PUSH!"
        Holding -> "HOLD!"

drawActiveBoost :: [Boost] -> Picture
drawActiveBoost [] = blank
drawActiveBoost [b] = colored (color b) (drawNumber $ roundTo 2 $ duration b)
drawActiveBoost (b : _) = drawActiveBoost [b]

drawActiveBoosts :: [Boost] -> Picture
drawActiveBoosts boosts =
    translated
        (screenWidth / 2 - 3)
        (-screenHeight / 2 + 1)
        ( slowMotionDuration
            <> immunityDuration
            <> forcingDuration
        )
  where
    slowMotionBoosts = maximumByDurationList [b | b@(SlowMotion{}) <- boosts]
    immunityBoosts = maximumByDurationList [b | b@(Immunity{}) <- boosts]
    forcingBoosts = maximumByDurationList [b | b@(Forcing{}) <- boosts]

    slowMotionDuration = drawActiveBoost slowMotionBoosts
    immunityDuration =
        translated 0 1 (drawActiveBoost immunityBoosts)
    forcingDuration =
        translated 0 2 (drawActiveBoost forcingBoosts)

drawGameObjects :: (GameObject g) => Double -> [g] -> Picture
drawGameObjects offset objects = pictures $ map draw onScreenObjects
  where
    onScreenObjects = takeWhile (onScreen offset) objects

drawBoosts :: (BoostObject b) => Double -> [b] -> Picture
drawBoosts offset b = drawGameObjects offset $ filter (not . hidden) b

drawPlayer :: Player -> Bool -> Bool -> WorldState -> Picture
drawPlayer Player{y = y, hitBoxSize = playerSize} immunity inverseGravity state =
    translated 0 y (shield <> maybeDeadPlayerPicture)
  where
    shield = if immunity then colored (translucent yellow) $ solidCircle playerSize else blank
    basePlayerPicture = lettering "\x1F6F8"
    playerPicture = if inverseGravity then rotated 3.14 basePlayerPicture else basePlayerPicture
    maybeDeadPlayerPicture = case state of
      Fail -> rotated 3.14 playerPicture
      _ -> playerPicture

drawScore :: Int -> Picture
drawScore score = translated (-screenWidth / 2 + 1) (-screenHeight / 2 + 1) (drawNumber score)

drawNumber :: (Show a) => a -> Picture
drawNumber x = lettering $ T.pack $ show x

drawDebug :: World -> Picture
drawDebug
    World
        { speed = speed'
        , currentSpeed = currentSpeed'
        , activeBoosts = activeBoosts'
        , boosts = boosts'
        , offset = offset'
        , player = Player{y = playerY, hitBoxSize = playerSize}
        } =
        colored red (rectangle screenWidth screenHeight)
            <> colored blue (solidCircle 0.1)
            <> translated 0 (-7) (drawNumber $ roundTo 3 speed')
            <> translated 0 (-8) (drawNumber $ roundTo 3 currentSpeed')
            <> translated (-3) (-9) (drawNumber $ length activeBoosts')
            <> translated 0 (-9) (drawNumber $ length $ onScreenBoosts)
            <> translated playerShift playerY (rectangle playerSize playerSize)
            <> boostHitBoxes
      where
        onScreenBoosts = takeWhile (onScreen offset') (filter (not . hidden) boosts')
        drawBoostHitBox b = translated (offset' + offsetX b) (offsetY b) (rectangle boostSize boostSize)
          where
            boostSize = radius b * 2
        boostHitBoxes = pictures $ map drawBoostHitBox onScreenBoosts
