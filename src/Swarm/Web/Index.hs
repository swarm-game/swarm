{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Swarm.Web.Index where

import Control.Lens ((^.))
import Data.Foldable (traverse_)
import Data.IntMap qualified as IM
import Data.Text (Text)
import Lucid
import Text.Pretty.Simple (pShowNoColor)
import Witch

import Swarm.Game.CESK
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Web.Htmx

data Page = WelcomePage | RobotsPage | RobotPage Int | AboutPage
  deriving (Eq)

mainBody :: GameState -> Page -> Html ()
mainBody gs page =
  doctypehtml_ do
    head_ do
      title_ "Swarm"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      with (script_ mempty) [src_ "/dists/htmx/htmx.min.js"]
      with (script_ mempty) [src_ "/dists/tailwind/tailwind.min.js"]
    with body_ [id_ "main"] do
      componentNav page
      mkPage gs page

mkPage :: GameState -> Page -> Html ()
mkPage g = \case
  WelcomePage -> "Welcome"
  RobotsPage -> componentRobots g
  RobotPage rid -> componentRobot rid g
  AboutPage -> "About page"

pagePath :: Page -> Text
pagePath = \case
  WelcomePage -> "/"
  RobotsPage -> "/robots"
  RobotPage rid -> "/robot/" <> from (show rid)
  AboutPage -> "/about"

-- | The navigation header with top level links
componentNav :: Page -> Html ()
componentNav page =
  with' nav_ "bg-sky-700 p-1 shadow w-full flex" do
    with' div_ "flex-grow" do
      with' span_ "font-semibold text-white" do
        hxNavLink "/" Nothing "SWARM"
      navLink RobotsPage "Robots"
    div_ do
      navLink AboutPage "About"
 where
  navLink :: Page -> Text -> Html ()
  navLink navPage navName =
    let navLinkClass
          | navPage == page = " bg-slate-500"
          | otherwise = ""
        extra
          | navPage == AboutPage = " right"
          | otherwise = ""
        linkClass = "m-4 p-1 cursor-pointer text-white rounded hover:text-teal-500" <> navLinkClass <> extra
     in hxNavLink (pagePath navPage) (Just linkClass) (toHtml navName)

-- | Display a single robot information
componentRobot :: Int -> GameState -> Html ()
componentRobot rid g = do
  with' div_ "grid p-4 place-content-center" do
    with' span_ "font-semibold pb-3" do
      "Robot " <> toHtml (show rid)
    with' pre_ "whitespace-pre-wrap" do
      case IM.lookup rid (g ^. robotMap) of
        Nothing -> "Unknown robot"
        Just r -> toHtml (pShowNoColor r)

-- | Display the list of robots
componentRobots :: GameState -> Html ()
componentRobots g = do
  with' div_ "grid p-4 place-content-center" do
    with' span_ "font-semibold pb-3" do
      "Robots"
    with' div_ "not-prose bg-slate-50 border rounded-xl w-80" do
      with' table_ "table-auto border-collapse w-80" do
        thead_ $ with' tr_ "border-b text-left" do
          traverse_ (with' th_ "p-1") ["Name", "Info"]
        with' tbody_ "bg-white" do
          traverse_ mkRobotRow robots
 where
  mkRobotRow robot = do
    with' tr_ "border-b" do
      td' $ hxNavLink (pagePath $ RobotPage $ robot ^. robotID) Nothing (toHtml $ robot ^. robotName)
      td' $ case robot ^. machine of
        Waiting {} -> "waiting"
        _
          | isActive robot -> "busy"
          | otherwise -> "idle"

  robots = IM.elems $ g ^. robotMap
  td' = with' td_ "p-1" . toHtml
