module Swarm.Util where

import           Data.Bifunctor  (bimap)
import           Data.Text       (Text)

import           Swarm.AST
import           Swarm.Parse
import           Swarm.Pretty
import           Swarm.Typecheck

processCmd :: Text -> Either Text Term
processCmd txt = do
  t <- readTerm txt
  bimap renderPretty (const t) (check t TyCmd)

