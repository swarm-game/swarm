{-# LANGUAGE TemplateHaskell #-}

module Swarm.Types where

import           Control.Lens
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Text    (Text)
import           Linear

import           Brick        hiding (Direction)
import           Brick.Focus
import           Brick.Forms

import           Swarm.AST

data Tick = Tick

data Robot = Robot
  { _location     :: V2 Int
  , _direction    :: V2 Int
  , _robotProgram :: Program
  , _static       :: Bool
  }
  deriving (Eq, Ord, Show)

mkBase :: Command -> Robot
mkBase cmd = Robot (V2 0 0) (V2 0 0) [cmd] True

data Item = Resource Char
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _robots    :: [Robot]
  , _newRobots :: [Robot]
  , _world     :: [[Char]]
  , _inventory :: Map Item Int
  }

data Name
  = REPLPanel
  | WorldPanel
  | InfoPanel
  | REPLInput
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data UIState = UIState
  { _uiFocusRing   :: FocusRing Name
  , _uiReplForm    :: Form Text Tick Name
  , _uiReplHistory :: [Text]
  , _uiReplHistIdx :: Int
  , _uiError       :: Maybe (Widget Name)
  }

makeLenses ''Robot
makeLenses ''GameState
makeLenses ''UIState
