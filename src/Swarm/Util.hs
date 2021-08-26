module Swarm.Util where

import           Data.Bifunctor  (first)
import qualified Data.Map        as M
import           Data.Text       (Text)

import           Swarm.AST
import           Swarm.Parse
import           Swarm.Pretty
import           Swarm.Typecheck
import           Swarm.Types

processCmd :: Text -> Either Text ATerm
processCmd txt = do
  t <- readTerm txt
  first renderPretty (check M.empty t (TyCmd TyUnit))

