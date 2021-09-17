-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.State
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definition of the record holding all the game-related state, and various related
-- utility functions.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.Game.State
  ( -- * Game state record

    ViewCenterRule(..), GameMode(..), REPLStatus(..)

  , GameState, initGameState

    -- ** GameState fields

  , gameMode, paused, robotMap, newRobots, gensym
  , entityMap, recipesOut, recipesIn, world
  , viewCenterRule, viewCenter
  , needsRedraw, replStatus, messageQueue

    -- * Utilities for modifying the game state

  , applyViewCenterRule
  , recalcViewCenter
  , modifyViewCenter

  , focusedRobot
  , ensureUniqueName
  , addRobot

  , emitMessage
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor       (first)
import           Data.IntMap          (IntMap)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Linear
import           Witch

import           Swarm.Game.Entity
import           Swarm.Game.Recipe
import           Swarm.Game.Robot
import           Swarm.Game.Value
import qualified Swarm.Game.World     as W
import           Swarm.Game.WorldGen  (findGoodOrigin, testWorld2)
import           Swarm.Language.Types
import           Swarm.Util

-- | The 'ViewCenterRule' specifies how to determine the center of the
--   world viewport.
data ViewCenterRule
  = VCLocation (V2 Int)   -- ^ The view should be centered on an absolute position.
  | VCRobot Text          -- ^ The view should be centered on a certain robot.
  deriving (Eq, Ord, Show)

makePrisms ''ViewCenterRule

-- | The game mode determines various aspects of how the game works.
--   At the moment, there are only two modes, but more will be added
--   in the future.
data GameMode
  = Classic    -- ^ Explore an open world, gather resources, and upgrade your programming abilities.
  | Creative   -- ^ Like 'Classic' mode, but there are no constraints on the programs you can write.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A data type to represent the current status of the REPL.
data REPLStatus
  = REPLDone
    -- ^ The REPL is not doing anything actively at the moment.
  | REPLWorking Polytype (Maybe Value)
    -- ^ A command entered at the REPL is currently being run.  The
    --   'Polytype' represents the type of the expression that was
    --   entered.  The @Maybe Value@ starts out as @Nothing@ and gets
    --   filled in with a result once the command completes.
  deriving (Eq, Show)

-- | The main record holding the state for the game itself (as
--   distinct from the UI).
data GameState = GameState
  { _gameMode       :: GameMode
  , _paused         :: Bool
  , _robotMap       :: Map Text Robot
  , _newRobots      :: [Robot]
  , _gensym         :: Int
  , _entityMap      :: EntityMap
  , _recipesOut     :: IntMap [Recipe Entity]
  , _recipesIn      :: IntMap [Recipe Entity]
  , _world          :: W.TileCachingWorld Int Entity
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter     :: V2 Int
  , _needsRedraw    :: Bool
  , _replStatus     :: REPLStatus
  , _messageQueue   :: [Text]
  }

let exclude = ['_viewCenter] in
  makeLensesWith
    (lensRules
       & generateSignatures .~ False
       & lensField . mapped . mapped %~ \fn n ->
         if n `elem` exclude then [] else fn n)
    ''GameState

-- | The current 'GameMode'.
gameMode :: Lens' GameState GameMode

-- | Whether the game is currently paused.
paused :: Lens' GameState Bool

-- | All the robots that currently exist in the game, indexed by name.
robotMap :: Lens' GameState (Map Text Robot)

-- | A temporary place to hold any new robots created during a tick. XXX get rid of this?
newRobots :: Lens' GameState [Robot]

-- | A counter used to generate globally unique IDs.
gensym :: Lens' GameState Int

-- | The catalog of all entities that the game knows about.
entityMap :: Lens' GameState EntityMap

-- | All recipes the game knows about, indexed by outputs.
recipesOut :: Lens' GameState (IntMap [Recipe Entity])

-- | All recipes the game knows about, indexed by inputs.
recipesIn :: Lens' GameState (IntMap [Recipe Entity])

-- | The current state of the world (terrain and entities only; robots
--   are stored in the 'robotMap').
world :: Lens' GameState (W.TileCachingWorld Int Entity)

-- | The current rule for determining the center of the world view.
viewCenterRule :: Lens' GameState ViewCenterRule

-- | The current center of the world view. Note that this cannot be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenter :: Getter GameState (V2 Int)
viewCenter = to _viewCenter

-- | Whether the world view needs to be redrawn.
needsRedraw :: Lens' GameState Bool

-- | The current status of the REPL.
replStatus :: Lens' GameState REPLStatus

-- | A queue of global messages.
messageQueue :: Lens' GameState [Text]

-- | Given a current mapping from robot names to robots, apply a
--   'ViewCenterRule' to derive the location it refers to.  The result
--   is @Maybe@ because the rule may refer to a robot which does not
--   exist.
applyViewCenterRule :: ViewCenterRule -> Map Text Robot -> Maybe (V2 Int)
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . robotLocation

-- | Recalculate the veiw center (and cache the result in the
--   'viewCenter' field) based on the current 'viewCenterRule'.  If
--   the 'viewCenterRule' specifies a robot which does not exist,
--   simply leave the current 'viewCenter' as it is. Set 'needsRedraw'
--   if the view center changes.
recalcViewCenter :: GameState -> GameState
recalcViewCenter g = g
  { _viewCenter = newViewCenter }
  & (if newViewCenter /= oldViewCenter then needsRedraw .~ True else id)
  where
    oldViewCenter = g ^. viewCenter
    newViewCenter = fromMaybe oldViewCenter (applyViewCenterRule (g ^. viewCenterRule) (g ^. robotMap))

-- | Modify the 'viewCenter' by applying an arbitrary function to the
--   current value.  Note that this also modifies the 'viewCenterRule'
--   to match.  After calling this function the 'viewCenterRule' will
--   specify a particular location, not a robot.
modifyViewCenter :: (V2 Int -> V2 Int) -> GameState -> GameState
modifyViewCenter update g = g
  & case g ^. viewCenterRule of
      VCLocation l -> viewCenterRule .~ VCLocation (update l)
      VCRobot _    -> viewCenterRule .~ VCLocation (update (g ^. viewCenter))
  & recalcViewCenter

-- | Find out which robot is currently specified by the
--   'viewCenterRule', if any.
focusedRobot :: GameState -> Maybe Robot
focusedRobot g = do
  focusedRobotName <- g ^? viewCenterRule . _VCRobot
  g ^? robotMap . ix focusedRobotName

-- | Given a 'Robot', possibly modify its name to ensure that the name
--   is unique among robots.  This is done simply by appending a new unique
ensureUniqueName :: MonadState GameState m => Robot -> m Robot
ensureUniqueName newRobot = do
  let name = newRobot ^. robotName
  newName <- uniquifyRobotName name Nothing
  return $ newRobot & robotName .~ newName

uniquifyRobotName :: MonadState GameState m => Text -> Maybe Int -> m Text
uniquifyRobotName name tag = do
  let name' = name `T.append` maybe "" (into @Text . show) tag
  collision <- uses robotMap (M.member name')
  case collision of
    True -> do
      tag' <- gensym <+= 1
      uniquifyRobotName name (Just tag')
    False -> return name'

addRobot :: MonadState GameState m => Robot -> m Robot
addRobot r = do
  r' <- ensureUniqueName r
  newRobots %= (r' :)
  return r'

initGameState :: ExceptT Text IO GameState
initGameState = do
  liftIO $ putStrLn "Loading entities..."
  entities <- loadEntities >>= (`isRightOr` id)
  liftIO $ putStrLn "Loading recipes..."
  recipes <- loadRecipes entities >>= (`isRightOr` id)

  let baseDeviceNames = ["solar panel", "3D printer", "dictionary", "workbench", "grabber"]
      baseDevices = mapMaybe (`lookupEntityName` entities) baseDeviceNames

  return $ GameState
    { _gameMode       = Classic
    , _paused         = False
    , _robotMap       = M.singleton "base" (baseRobot baseDevices)
    , _newRobots      = []
    , _gensym         = 0
    , _entityMap      = entities
    , _recipesOut     = outRecipeMap recipes
    , _recipesIn      = inRecipeMap recipes
    , _world          =
      W.newWorld . fmap ((lkup entities <$>) . first fromEnum) . findGoodOrigin $ testWorld2
    , _viewCenterRule = VCRobot "base"
    , _viewCenter     = V2 0 0
    , _needsRedraw    = False
    , _replStatus     = REPLDone
    , _messageQueue   = []
    }
  where
    lkup :: EntityMap -> Maybe Text -> Maybe Entity
    lkup _  Nothing  = Nothing
    lkup em (Just t) = lookupEntityName t em

maxMessageQueueSize :: Int
maxMessageQueueSize = 1000

emitMessage :: MonadState GameState m => Text -> m ()
emitMessage msg = do
  q <- use messageQueue
  messageQueue %= (msg:) . (if length q >= maxMessageQueueSize then init else id)
