{-# LANGUAGE TypeOperators #-}
module Swarm.Util where

import           Data.Maybe (fromMaybe)

infixr 1 ?
(?) :: Maybe a -> a -> a
(?) = flip fromMaybe

data a ::: b = a ::: b
