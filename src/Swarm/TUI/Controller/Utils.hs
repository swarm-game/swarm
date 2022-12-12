{-# LANGUAGE PatternSynonyms #-}

module Swarm.TUI.Controller.Utils where

import Brick hiding (Direction)
import Graphics.Vty qualified as V

-- | Pattern synonyms to simplify brick event handler
pattern Key :: V.Key -> BrickEvent n e
pattern Key k = VtyEvent (V.EvKey k [])

pattern CharKey, ControlChar, MetaChar :: Char -> BrickEvent n e
pattern CharKey c = VtyEvent (V.EvKey (V.KChar c) [])
pattern ControlChar c = VtyEvent (V.EvKey (V.KChar c) [V.MCtrl])
pattern MetaChar c = VtyEvent (V.EvKey (V.KChar c) [V.MMeta])

pattern ShiftKey :: V.Key -> BrickEvent n e
pattern ShiftKey k = VtyEvent (V.EvKey k [V.MShift])

pattern EscapeKey :: BrickEvent n e
pattern EscapeKey = VtyEvent (V.EvKey V.KEsc [])

pattern FKey :: Int -> BrickEvent n e
pattern FKey c = VtyEvent (V.EvKey (V.KFun c) [])
