module Swarm.Util where

import           Data.Bifunctor  (bimap)
import qualified Data.Map        as M
import           Data.Text       (Text)

import           Swarm.AST
import           Swarm.Parse
import           Swarm.Pretty
import           Swarm.Typecheck
import           Swarm.Types

processCmd :: Text -> Either Text Term
processCmd txt = do
  t <- readTerm txt
  bimap renderPretty (const t) (check M.empty t TyCmd)

