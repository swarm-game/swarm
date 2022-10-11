{-# LANGUAGE TemplateHaskell #-}

module Swarm.Language.Typed (Typed, value, polytype, requires) where
import Swarm.Language.Types (Polytype)
import Swarm.Language.Requirement (Requirements)
import Control.Lens ( makeLenses ) 

-- | A value, or a hole, or something else that has its type & requirementss fixed
data Typed v = Typed
  { _value :: v
  , _polytype :: Polytype
  , _requires :: Requirements
  }

makeLenses ''Typed
