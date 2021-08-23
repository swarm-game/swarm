{-# LANGUAGE TemplateHaskell #-}

module Swarm.UI where

import           Control.Lens
import           Data.Text    (Text)

import           Brick        hiding (Direction)
import           Brick.Focus
import           Brick.Forms

data Tick = Tick

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

makeLenses ''UIState
