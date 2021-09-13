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

module Swarm.Game.State where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor       (first)
import           Data.IntMap          (IntMap)
import           Data.Map             (Map, (!))
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
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

data ViewCenterRule
  = VCLocation (V2 Int)
  | VCRobot Text
  deriving (Eq, Ord, Show)

makePrisms ''ViewCenterRule

data GameMode
  = Classic
  | Creative
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data REPLResult
  = REPLDone
  | REPLWorking Polytype (Maybe Value)
  deriving (Eq, Show)

data GameState = GameState
  { _gameMode       :: GameMode
  , _paused         :: Bool
  , _robotMap       :: Map Text Robot
  , _newRobots      :: [Robot]
  , _gensym         :: Int
  , _entityMap      :: Map Text Entity
  , _recipesOut     :: IntMap [Recipe Entity]
  , _recipesIn      :: IntMap [Recipe Entity]
  , _world          :: W.TileCachingWorld Int Entity
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter     :: V2 Int
  , _updated        :: Bool
  , _replResult     :: REPLResult
  , _messageQueue   :: [Text]
  }

makeLenses ''GameState

applyViewCenterRule :: ViewCenterRule -> Map Text Robot -> Maybe (V2 Int)
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . robotLocation

updateViewCenter :: GameState -> GameState
updateViewCenter g = g
  & viewCenter .~ newViewCenter
  & (if newViewCenter /= oldViewCenter then updated .~ True else id)
  where
    oldViewCenter = g ^. viewCenter
    newViewCenter = fromMaybe oldViewCenter (applyViewCenterRule (g ^. viewCenterRule) (g ^. robotMap))

manualViewCenterUpdate :: (V2 Int -> V2 Int) -> GameState -> GameState
manualViewCenterUpdate update g = g
  & case g ^. viewCenterRule of
      VCLocation l -> viewCenterRule .~ VCLocation (update l)
      VCRobot _    -> viewCenterRule .~ VCLocation (update (g ^. viewCenter))
  & updateViewCenter

focusedRobot :: GameState -> Maybe Robot
focusedRobot g = do
  focusedRobotName <- g ^? viewCenterRule . _VCRobot
  g ^? robotMap . ix focusedRobotName

ensureUniqueName :: MonadState GameState m => Robot -> m Robot
ensureUniqueName newRobot = do
  -- See if another robot already has the same name...
  let name = newRobot ^. robotName
  collision <- uses robotMap (M.member name)
  case collision of
    -- If so, add a suffix to make the name unique.
    True -> do
      tag <- gensym <+= 1
      let name' = name `T.append` into @Text (show tag)
      return $ newRobot & robotName .~ name'
    False -> return newRobot

addRobot :: MonadState GameState m => Robot -> m Robot
addRobot r = do
  r' <- ensureUniqueName r
  newRobots %= (r' :)
  return r'

  -- res <- runExceptT $ do
  --   liftIO $ putStrLn "Loading entities..."
  --   es <- loadEntities >>= (`isRightOr` id)
  --   liftIO $ putStrLn "Loading recipes..."
  --   rs <- loadRecipes es >>= (`isRightOr` id)

  -- case res of
  --   Left errMsg -> T.putStrLn errMsg
  --   Right ()    -> return ()


initGameState :: ExceptT Text IO GameState
initGameState = do
  liftIO $ putStrLn "Loading entities..."
  entities <- loadEntities >>= (`isRightOr` id)
  liftIO $ putStrLn "Loading recipes..."
  recipes <- loadRecipes entities >>= (`isRightOr` id)

  return $ GameState
    { _gameMode       = Classic
    , _paused         = False
    , _robotMap       = M.singleton "base" (baseRobot [entities!"solar panel"])  -- XXX !
    , _newRobots      = []
    , _gensym         = 0
    , _entityMap      = entities
    , _recipesOut     = outRecipeMap recipes
    , _recipesIn      = inRecipeMap recipes
    , _world          =
      W.newWorld . fmap ((lkup entities <$>) . first fromEnum) . findGoodOrigin $ testWorld2
    , _viewCenterRule = VCRobot "base"
    , _viewCenter     = V2 0 0
    , _updated        = False
    , _replResult     = REPLDone
    , _messageQueue   = []
    }
  where
    lkup :: Map Text Entity -> Maybe Text -> Maybe Entity
    lkup _  Nothing  = Nothing
    lkup em (Just t) = M.lookup t em

maxMessageQueueSize :: Int
maxMessageQueueSize = 1000

emitMessage :: MonadState GameState m => Text -> m ()
emitMessage msg = do
  q <- use messageQueue
  messageQueue %= (msg:) . (if length q >= maxMessageQueueSize then init else id)
