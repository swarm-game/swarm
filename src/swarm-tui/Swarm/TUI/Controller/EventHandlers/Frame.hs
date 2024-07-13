-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Handling 'Swarm.TUI.Model.Frame' events.
module Swarm.TUI.Controller.EventHandlers.Frame (
  runFrameUI,
  runGameTickUI,

  -- ** Constants
  ticksPerFrameCap,
) where

import Brick
import Control.Lens as Lens
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits
import Data.Map qualified as M
import Swarm.Game.Achievement.Attainment (achievement)
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.Game.State
import Swarm.Game.State.Substate
import Swarm.Game.Step (gameTick)
import Swarm.TUI.Controller.UpdateUI
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.Achievements (popupAchievement)
import Swarm.TUI.Model.UI
import System.Clock

ticksPerFrameCap :: Int
ticksPerFrameCap = 30

-- | Run the game for a single /frame/ (/i.e./ screen redraw), then
--   update the UI.  Depending on how long it is taking to draw each
--   frame, and how many ticks per second we are trying to achieve,
--   this may involve stepping the game any number of ticks (including
--   zero).
runFrameUI :: Bool -> EventM Name AppState ()
runFrameUI forceRedraw = do
  runFrame
  redraw <- updateUI
  unless (forceRedraw || redraw) continueWithoutRedraw

-- | Run the game for a single frame, without updating the UI.
runFrame :: EventM Name AppState ()
runFrame = do
  -- Reset the needsRedraw flag.  While processing the frame and stepping the robots,
  -- the flag will get set to true if anything changes that requires redrawing the
  -- world (e.g. a robot moving or disappearing).
  gameState . needsRedraw .= False

  -- The logic here is taken from https://gafferongames.com/post/fix_your_timestep/ .

  -- Find out how long the previous frame took, by subtracting the
  -- previous time from the current time.
  prevTime <- use (uiState . uiGameplay . uiTiming . lastFrameTime)
  curTime <- liftIO $ getTime Monotonic
  let frameTime = diffTimeSpec curTime prevTime

  -- Remember now as the new previous time.
  uiState . uiGameplay . uiTiming . lastFrameTime .= curTime

  -- We now have some additional accumulated time to play with.  The
  -- idea is to now "catch up" by doing as many ticks as are supposed
  -- to fit in the accumulated time.  Some accumulated time may be
  -- left over, but it will roll over to the next frame.  This way we
  -- deal smoothly with things like a variable frame rate, the frame
  -- rate not being a nice multiple of the desired ticks per second,
  -- etc.
  uiState . uiGameplay . uiTiming . accumulatedTime += frameTime

  -- Figure out how many ticks per second we're supposed to do,
  -- and compute the timestep `dt` for a single tick.
  lgTPS <- use (uiState . uiGameplay . uiTiming . lgTicksPerSecond)
  let oneSecond = 1_000_000_000 -- one second = 10^9 nanoseconds
      dt
        | lgTPS >= 0 = oneSecond `div` (1 `shiftL` lgTPS)
        | otherwise = oneSecond * (1 `shiftL` abs lgTPS)

  -- Update TPS/FPS counters every second
  infoUpdateTime <- use (uiState . uiGameplay . uiTiming . lastInfoTime)
  let updateTime = toNanoSecs $ diffTimeSpec curTime infoUpdateTime
  when (updateTime >= oneSecond) $ do
    -- Wait for at least one second to have elapsed
    when (infoUpdateTime /= 0) $ do
      -- set how much frame got processed per second
      frames <- use (uiState . uiGameplay . uiTiming . frameCount)
      uiState . uiGameplay . uiTiming . uiFPS .= fromIntegral (frames * fromInteger oneSecond) / fromIntegral updateTime

      -- set how much ticks got processed per frame
      uiTicks <- use (uiState . uiGameplay . uiTiming . tickCount)
      uiState . uiGameplay . uiTiming . uiTPF .= fromIntegral uiTicks / fromIntegral frames

      -- ensure this frame gets drawn
      gameState . needsRedraw .= True

    -- Reset the counter and wait another seconds for the next update
    uiState . uiGameplay . uiTiming . tickCount .= 0
    uiState . uiGameplay . uiTiming . frameCount .= 0
    uiState . uiGameplay . uiTiming . lastInfoTime .= curTime

  -- Increment the frame count
  uiState . uiGameplay . uiTiming . frameCount += 1

  -- Now do as many ticks as we need to catch up.
  uiState . uiGameplay . uiTiming . frameTickCount .= 0
  runFrameTicks (fromNanoSecs dt)

-- | Do zero or more ticks, with each tick notionally taking the given
--   timestep, until we have used up all available accumulated time,
--   OR until we have hit the cap on ticks per frame, whichever comes
--   first.
runFrameTicks :: TimeSpec -> EventM Name AppState ()
runFrameTicks dt = do
  a <- use (uiState . uiGameplay . uiTiming . accumulatedTime)
  t <- use (uiState . uiGameplay . uiTiming . frameTickCount)

  -- Ensure there is still enough time left, and we haven't hit the
  -- tick limit for this frame.
  when (a >= dt && t < ticksPerFrameCap) $ do
    -- If so, do a tick, count it, subtract dt from the accumulated time,
    -- and loop!
    runGameTick
    Brick.zoom (uiState . uiGameplay . uiTiming) $ do
      tickCount += 1
      frameTickCount += 1
      accumulatedTime -= dt
    runFrameTicks dt

-- | Run the game for a single tick, and update the UI.
runGameTickUI :: EventM Name AppState ()
runGameTickUI = runGameTick >> void updateUI

updateAchievements :: EventM Name AppState ()
updateAchievements = do
  -- Merge the in-game achievements with the master list in UIState
  achievementsFromGame <- use $ gameState . discovery . gameAchievements
  let wrappedGameAchievements = M.mapKeys GameplayAchievement achievementsFromGame

  oldMasterAchievementsList <- use $ uiState . uiAchievements
  uiState . uiAchievements %= M.unionWith (<>) wrappedGameAchievements

  -- Don't save to disk unless there was a change in the attainment list.
  let incrementalAchievements = wrappedGameAchievements `M.difference` oldMasterAchievementsList
  unless (null incrementalAchievements) $ do
    mapM_ (popupAchievement . view achievement) incrementalAchievements

    newAchievements <- use $ uiState . uiAchievements
    liftIO $ saveAchievementsInfo $ M.elems newAchievements

-- | Run the game for a single tick (/without/ updating the UI).
--   Every robot is given a certain amount of maximum computation to
--   perform a single world action (like moving, turning, grabbing,
--   etc.).
runGameTick :: EventM Name AppState ()
runGameTick = do
  ticked <- zoomGameState gameTick
  when ticked updateAchievements
