-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Sum type representing the Swarm debug options.
module Swarm.TUI.Model.DebugOption (
  DebugOption (..),
  debugOptionName,
  debugOptionDescription,
  readDebugOption,
  readDebugOptionList,
) where

import Data.Foldable (find, foldl')
import Data.List.Extra (enumerate, splitOn, trim)

data DebugOption
  = ToggleCreative
  | ToggleWorldEditor
  | DebugCESK
  | ListAllRobots
  | ListRobotIDs
  | ShowHiddenGoals
  | ShowGoalDialogsInAutoPlay
  | LoadTestingScenarios
  deriving (Eq, Ord, Show, Enum, Bounded)

debugOptionName :: DebugOption -> String
debugOptionName = \case
  ToggleCreative -> "creative"
  ToggleWorldEditor -> "editor"
  DebugCESK -> "cesk"
  ListAllRobots -> "all_robots"
  ListRobotIDs -> "robot_id"
  ShowHiddenGoals -> "hidden_goals"
  ShowGoalDialogsInAutoPlay -> "autoplay_goals"
  LoadTestingScenarios -> "testing"

debugOptionDescription :: DebugOption -> String
debugOptionDescription = \case
  ToggleCreative -> "allow toggling creative mode on/off"
  ToggleWorldEditor -> "allow toggling the world editor mode on/off"
  DebugCESK -> "allow toggling the CESK debug view on/off"
  ListAllRobots -> "list all robots (including system robots) in the robot panel"
  ListRobotIDs -> "list robot IDs in the robot panel"
  ShowHiddenGoals -> "show hidden objectives in the goal dialog"
  ShowGoalDialogsInAutoPlay -> "show goal dialogs when running in autoplay"
  LoadTestingScenarios -> "load Testing folder in scenarios menu"

readDebugOption :: String -> Maybe DebugOption
readDebugOption name = find ((trim name ==) . debugOptionName) enumerate

readDebugOptionList :: String -> Either String [DebugOption]
readDebugOptionList = foldl' eitherRead (Right []) . splitOn ","
 where
  eitherRead s o = case (s, readDebugOption o) of
    (Left e, _) -> Left e
    (_, Nothing) -> Left $ "unknown option '" <> o <> "'"
    (Right oss, Just os) -> Right $ os : oss
