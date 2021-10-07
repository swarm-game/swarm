-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Swarm.Game.State
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definition of the record holding all the game-related state, and various related
-- utility functions.
module Swarm.Game.State (
  -- * Game state record
  ViewCenterRule (..),
  GameMode (..),
  REPLStatus (..),
  RunStatus (..),
  GameState,
  Seed,
  initGameState,

  -- ** GameState fields
  gameMode,
  runStatus,
  paused,
  robotMap,
  activeRobots,
  gensym,
  randGen,
  entityMap,
  recipesOut,
  recipesIn,
  world,
  viewCenterRule,
  viewCenter,
  needsRedraw,
  replStatus,
  replWorking,
  messageQueue,
  focusedRobotName,
  ticks,

  -- * Utilities
  applyViewCenterRule,
  recalcViewCenter,
  modifyViewCenter,
  viewingRegion,
  focusedRobot,
  clearFocusedRobotLogUpdated,
  ensureUniqueName,
  addRobot,
  emitMessage,
  sleepUntil,
  wakeUpRobotsDoneSleeping,
  deleteRobot,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Linear
import Witch (into)

import Data.Set (Set)
import qualified Data.Set as S
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.Value
import qualified Swarm.Game.World as W
import Swarm.Game.WorldGen (Seed, findGoodOrigin, testWorld2)
import Swarm.Language.Types
import Swarm.Util
import System.Random (StdGen, mkStdGen)

-- | The 'ViewCenterRule' specifies how to determine the center of the
--   world viewport.
data ViewCenterRule
  = -- | The view should be centered on an absolute position.
    VCLocation (V2 Int64)
  | -- | The view should be centered on a certain robot.
    VCRobot Text
  deriving (Eq, Ord, Show)

makePrisms ''ViewCenterRule

-- | The game mode determines various aspects of how the game works.
--   At the moment, there are only two modes, but more will be added
--   in the future.
data GameMode
  = -- | Explore an open world, gather resources, and upgrade your programming abilities.
    Classic
  | -- | Like 'Classic' mode, but there are no constraints on the programs you can write.
    Creative
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A data type to represent the current status of the REPL.
data REPLStatus
  = -- | The REPL is not doing anything actively at the moment.
    REPLDone
  | -- | A command entered at the REPL is currently being run.  The
    --   'Polytype' represents the type of the expression that was
    --   entered.  The @Maybe Value@ starts out as @Nothing@ and gets
    --   filled in with a result once the command completes.
    REPLWorking Polytype (Maybe Value)
  deriving (Eq, Show)

-- | A data type to keep track of the pause mode.
data RunStatus
  = -- | The game is running.
    Running
  | -- | The user paused the game, and it should stay pause after visiting the help.
    ManualPause
  | -- | The game got paused while visiting the help,
    --   and it should unpause after returning back to the game.
    AutoPause
  deriving (Eq, Show)

-- | The main record holding the state for the game itself (as
--   distinct from the UI).  See the lenses below for access to its
--   fields.
data GameState = GameState
  { _gameMode :: GameMode
  , _runStatus :: RunStatus
  , _robotMap :: Map Text Robot
  , _activeRobots :: Set Text
  , -- Waiting robots for a given time are currently a list because it is cheaper
    -- to append to a list than to a Set, and we currently never delete waiting
    -- robots. If deleting waiting robots becomes a thing and is frequent enough,
    -- then it may be worth switching to a Set.
    _waitingRobots :: Map Integer [Text]
  , _gensym :: Int
  , _randGen :: StdGen
  , _entityMap :: EntityMap
  , _recipesOut :: IntMap [Recipe Entity]
  , _recipesIn :: IntMap [Recipe Entity]
  , _world :: W.World Int Entity
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter :: V2 Int64
  , _needsRedraw :: Bool
  , _replStatus :: REPLStatus
  , _messageQueue :: [Text]
  , _focusedRobotName :: Text
  , _ticks :: Integer
  }

-- We want to access _activeRobots via a Lens inside this module but to expose
-- it as a Getter externally to protect invariants.
makeLensesFor [("_activeRobots", "internalActiveRobots")] ''GameState

let exclude = ['_viewCenter, '_focusedRobotName, '_viewCenterRule, '_activeRobots]
 in makeLensesWith
      ( lensRules
          & generateSignatures .~ False
          & lensField . mapped . mapped %~ \fn n ->
            if n `elem` exclude then [] else fn n
      )
      ''GameState

-- | The current 'GameMode'.
gameMode :: Lens' GameState GameMode

-- | The current 'RunStatus'.
runStatus :: Lens' GameState RunStatus

-- | Whether the game is currently paused.
paused :: Getter GameState Bool
paused = to (\s -> s ^. runStatus /= Running)

-- | All the robots that currently exist in the game, indexed by name.
robotMap :: Lens' GameState (Map Text Robot)

-- | The names of the robots that are currently not sleeping.
activeRobots :: Getter GameState (Set Text)
activeRobots = internalActiveRobots

-- | The names of the robots that are currently sleeping, indexed by wake up
-- | time. Internal.
waitingRobots :: Lens' GameState (Map Integer [Text])

-- | A counter used to generate globally unique IDs.
gensym :: Lens' GameState Int

-- | Pseudorandom generator initialized at start.
randGen :: Lens' GameState StdGen

-- | The catalog of all entities that the game knows about.
entityMap :: Lens' GameState EntityMap

-- | All recipes the game knows about, indexed by outputs.
recipesOut :: Lens' GameState (IntMap [Recipe Entity])

-- | All recipes the game knows about, indexed by inputs.
recipesIn :: Lens' GameState (IntMap [Recipe Entity])

-- | The current state of the world (terrain and entities only; robots
--   are stored in the 'robotMap').
world :: Lens' GameState (W.World Int Entity)

-- | The current rule for determining the center of the world view.
--   It updates also, viewCenter and focusedRobotName to keep
--   everything synchronize.
viewCenterRule :: Lens' GameState ViewCenterRule
viewCenterRule = lens getter setter
 where
  getter :: GameState -> ViewCenterRule
  getter = _viewCenterRule

  -- The setter takes care of updating viewCenter and focusedRobotName
  -- So non of this fields get out of sync.
  setter :: GameState -> ViewCenterRule -> GameState
  setter g rule =
    case rule of
      VCLocation v2 -> g {_viewCenterRule = rule, _viewCenter = v2}
      VCRobot txt ->
        let robotcenter = g ^? robotMap . ix txt <&> view robotLocation -- retrive the loc of the robot if it exist, Nothing otherwise.  sometimes, lenses are amazing...
         in case robotcenter of
              Nothing -> g
              Just v2 -> g {_viewCenterRule = rule, _viewCenter = v2, _focusedRobotName = txt}

-- | The current center of the world view. Note that this cannot be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenter :: Getter GameState (V2 Int64)
viewCenter = to _viewCenter

-- | Whether the world view needs to be redrawn.
needsRedraw :: Lens' GameState Bool

-- | The current status of the REPL.
replStatus :: Lens' GameState REPLStatus

-- | Whether the repl is currently working.
replWorking :: Getter GameState Bool
replWorking = to (\s -> matchesWorking $ s ^. replStatus)
 where
  matchesWorking REPLDone = False
  matchesWorking (REPLWorking _ _) = True

-- | A queue of global messages.
messageQueue :: Lens' GameState [Text]

-- | The current robot in focus. It is only a Getter because
--   this value should be updated only when viewCenterRule is.
focusedRobotName :: Getter GameState Text
focusedRobotName = to _focusedRobotName

-- | Given a current mapping from robot names to robots, apply a
--   'ViewCenterRule' to derive the location it refers to.  The result
--   is @Maybe@ because the rule may refer to a robot which does not
--   exist.
applyViewCenterRule :: ViewCenterRule -> Map Text Robot -> Maybe (V2 Int64)
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . robotLocation

-- | Recalculate the veiw center (and cache the result in the
--   'viewCenter' field) based on the current 'viewCenterRule'.  If
--   the 'viewCenterRule' specifies a robot which does not exist,
--   simply leave the current 'viewCenter' as it is. Set 'needsRedraw'
--   if the view center changes.
recalcViewCenter :: GameState -> GameState
recalcViewCenter g =
  g
    { _viewCenter = newViewCenter
    }
    & (if newViewCenter /= oldViewCenter then needsRedraw .~ True else id)
 where
  oldViewCenter = g ^. viewCenter
  newViewCenter = fromMaybe oldViewCenter (applyViewCenterRule (g ^. viewCenterRule) (g ^. robotMap))

-- | Modify the 'viewCenter' by applying an arbitrary function to the
--   current value.  Note that this also modifies the 'viewCenterRule'
--   to match.  After calling this function the 'viewCenterRule' will
--   specify a particular location, not a robot.
modifyViewCenter :: (V2 Int64 -> V2 Int64) -> GameState -> GameState
modifyViewCenter update g =
  g
    & case g ^. viewCenterRule of
      VCLocation l -> viewCenterRule .~ VCLocation (update l)
      VCRobot _ -> viewCenterRule .~ VCLocation (update (g ^. viewCenter))

-- | Given a width and height, compute the region, centered on the
--   'viewCenter', that should currently be in view.
viewingRegion :: GameState -> (Int64, Int64) -> (W.Coords, W.Coords)
viewingRegion g (w, h) = (W.Coords (rmin, cmin), W.Coords (rmax, cmax))
 where
  V2 cx cy = g ^. viewCenter
  (rmin, rmax) = over both (+ (- cy - h `div` 2)) (0, h -1)
  (cmin, cmax) = over both (+ (cx - w `div` 2)) (0, w -1)

-- | Find out which robot is currently specified by the
--   'viewCenterRule', if any.
focusedRobot :: GameState -> Maybe Robot
focusedRobot g = g ^? robotMap . ix (g ^. focusedRobotName)

-- | Clear the 'robotLogUpdated' flag of the focused robot.
clearFocusedRobotLogUpdated :: MonadState GameState m => m ()
clearFocusedRobotLogUpdated = do
  n <- use focusedRobotName
  robotMap . ix n . robotLogUpdated .= False

-- | Given a 'Robot', possibly modify its name to ensure that the name
--   is unique among robots.  This is done simply by appending a new unique
ensureUniqueName :: MonadState GameState m => Robot -> m Robot
ensureUniqueName newRobot = do
  let name = newRobot ^. robotName
  newName <- uniquifyRobotName name Nothing
  return $ newRobot & robotName .~ newName

-- | Given a robot name, possibly add a numeric suffix to the end to
--   ensure it is unique.
uniquifyRobotName :: MonadState GameState m => Text -> Maybe Int -> m Text
uniquifyRobotName name tag = do
  let name' = name `T.append` maybe "" (into @Text . show) tag
  collision <- uses robotMap (M.member name')
  case collision of
    True -> do
      tag' <- gensym <+= 1
      uniquifyRobotName name (Just tag')
    False -> return name'

-- | Add a robot to the game state, possibly updating its name to
--   ensure it is unique, and return the (possibly modified) robot.
addRobot :: MonadState GameState m => Robot -> m Robot
addRobot r = do
  r' <- ensureUniqueName r
  robotMap %= M.insert (r' ^. robotName) r'
  internalActiveRobots %= S.insert (r' ^. robotName)
  return r'

-- | Create an initial game state record, first loading entities and
--   recipies from disk.
initGameState :: Seed -> ExceptT Text IO GameState
initGameState seed = do
  liftIO $ putStrLn "Loading entities..."
  entities <- loadEntities >>= (`isRightOr` id)
  liftIO $ putStrLn "Loading recipes..."
  recipes <- loadRecipes entities >>= (`isRightOr` id)

  let baseDeviceNames =
        [ "solar panel"
        , "3D printer"
        , "dictionary"
        , "workbench"
        , "grabber"
        , "life support system"
        , "logger"
        ]
      baseDevices = mapMaybe (`lookupEntityName` entities) baseDeviceNames

  let baseName = "base"
  liftIO $ putStrLn ("Using seed... " <> show seed)

  return $
    GameState
      { _gameMode = Classic
      , _runStatus = Running
      , _robotMap = M.singleton baseName (baseRobot baseDevices)
      , _activeRobots = S.singleton baseName
      , _waitingRobots = M.empty
      , _gensym = 0
      , _randGen = mkStdGen seed
      , _entityMap = entities
      , _recipesOut = outRecipeMap recipes
      , _recipesIn = inRecipeMap recipes
      , _world =
          W.newWorld
            . fmap ((lkup entities <$>) . first fromEnum)
            . findGoodOrigin
            $ testWorld2 seed
      , _viewCenterRule = VCRobot baseName
      , _viewCenter = V2 0 0
      , _needsRedraw = False
      , _replStatus = REPLDone
      , _messageQueue = []
      , _focusedRobotName = baseName
      , _ticks = 0
      }
 where
  lkup :: EntityMap -> Maybe Text -> Maybe Entity
  lkup _ Nothing = Nothing
  lkup em (Just t) = lookupEntityName t em

maxMessageQueueSize :: Int
maxMessageQueueSize = 1000

-- | Add a message to the message queue.
emitMessage :: MonadState GameState m => Text -> m ()
emitMessage msg = do
  q <- use messageQueue
  messageQueue %= (msg :) . (if length q >= maxMessageQueueSize then init else id)

-- | The number of ticks elapsed since the game started.
ticks :: Lens' GameState Integer

-- | Takes a robot out of the activeRobots set and puts it in the waitingRobots
--   queue.
sleepUntil :: MonadState GameState m => Text -> Integer -> m ()
sleepUntil rn time = do
  internalActiveRobots %= S.delete rn
  waitingRobots . at time . non [] %= (rn :)

-- | Removes robots whose wake up time matches the current game ticks count
--   from the waitingRobots queue and put them back in the activeRobots set.
wakeUpRobotsDoneSleeping :: MonadState GameState m => m ()
wakeUpRobotsDoneSleeping = do
  time <- use ticks
  mrns <- waitingRobots . at time <<.= Nothing
  case mrns of
    Nothing -> return ()
    Just rns -> internalActiveRobots %= S.union (S.fromList rns)

deleteRobot :: MonadState GameState m => Text -> m ()
deleteRobot rn = do
  mrobot <- robotMap . at rn <<.= Nothing
  mrobot `forM_` \robot ->
    -- Currently the only way to delete a robot is to self-destruct, so
    -- deleteRobot should always be called on an active robot. But we prepare
    -- for the future.
    -- We could blindly delete the robot from both waitingRobots and activeRobots
    -- but deleting from waitingRobots is costly, even more so if you don't know
    -- the key.
    case waitingUntil robot of
      Nothing -> internalActiveRobots %= S.delete rn
      Just time -> waitingRobots . ix time %= filter (/= rn)
