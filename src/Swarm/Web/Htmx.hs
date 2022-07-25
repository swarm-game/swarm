{-# LANGUAGE OverloadedStrings #-}

-- | HTMX helpers
module Swarm.Web.Htmx where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)

-- | Create a navigation link with the htmx attributes
hxNavLink :: Text -> Maybe Text -> Html () -> Html ()
hxNavLink = hxNavLinkWithAttr []

hxNavLinkWithAttr :: [Attribute] -> Text -> Maybe Text -> Html () -> Html ()
hxNavLinkWithAttr xs url extraClass =
  with
    a_
    ( xs
        <> [ hxGet url
           , hxPushUrl
           , hxIndicator "#spinner"
           , hxTarget "#main"
           , class_ ("cursor-pointer hover:font-semibold" <> maybe "" (mappend " ") extraClass)
           , href_ url
           ]
    )

hxTrigger, hxTarget, hxGet, hxPost, hxIndicator :: Text -> Attribute
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxGet = makeAttribute "hx-get"
hxPost = makeAttribute "hx-post"
hxIndicator = makeAttribute "hx-indicator"

hxPushUrl :: Attribute
hxPushUrl = makeAttribute "hx-push-url" "true"

with' :: With a => a -> Text -> a
with' x n = with x [class_ n]
