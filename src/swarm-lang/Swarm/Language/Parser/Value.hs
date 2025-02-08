{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parse values of the Swarm language, indexed by type, by running the
-- full swarm-lang parser and then checking that the result is a value
-- of the proper type.
module Swarm.Language.Parser.Value (readValue) where

import Control.Lens ((^.))
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Key (parseKeyComboFull)
import Swarm.Language.Parser (readNonemptyTerm)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (checkTop)
import Swarm.Language.Types (Type)
import Swarm.Language.Value
import Text.Megaparsec qualified as MP

readValue :: Type -> Text -> Maybe Value
readValue ty txt = do
  -- Try to strip off a prefix representing a printable entity.  Look
  -- for the first colon or double quote.  We will ignore a colon if a
  -- double quote comes before it, because a colon could legitimately
  -- occur in a formatted Text value, e.g. "\"hi: there\"".  Otherwise,
  -- strip off anything occurring before the first colon.
  --
  -- Note, this would break if we ever had a printable entity whose
  -- name contains a colon; printing on such an entity would yield
  -- entity names like "Magic: The Gathering: 6" for which `read`, as
  -- implemented here, would not work correctly. However, that seems
  -- unlikely.
  let firstUnquotedColon = T.dropWhile (\c -> c /= ':' && c /= '"') txt
  let txt' = case T.uncons firstUnquotedColon of
        Nothing -> txt
        Just ('"', _) -> txt
        Just (':', t) -> t
        _ -> txt
  s <- eitherToMaybe $ readNonemptyTerm txt'
  _ <- eitherToMaybe $ checkTop Ctx.empty Ctx.empty Ctx.empty s ty
  toValue $ s ^. sTerm

toValue :: Term -> Maybe Value
toValue = \case
  TUnit -> Just VUnit
  TDir d -> Just $ VDir d
  TInt n -> Just $ VInt n
  TText t -> Just $ VText t
  TBool b -> Just $ VBool b
  TApp (TConst c) t2 -> case c of
    Neg -> toValue t2 >>= negateInt
    Inl -> VInj False <$> toValue t2
    Inr -> VInj True <$> toValue t2
    Key -> do
      VText k <- toValue t2
      VKey <$> eitherToMaybe (MP.runParser parseKeyComboFull "" k)
    _ -> Nothing
  TPair t1 t2 -> VPair <$> toValue t1 <*> toValue t2
  TRcd m -> VRcd <$> traverse (>>= toValue) m
  -- List the other cases explicitly, instead of a catch-all, so that
  -- we will get a warning if we ever add new constructors in the
  -- future
  TConst {} -> Nothing
  TAntiInt {} -> Nothing
  TAntiText {} -> Nothing
  TRequireDevice {} -> Nothing
  TRequire {} -> Nothing
  TRequirements {} -> Nothing
  TVar {} -> Nothing
  TLam {} -> Nothing
  TApp {} -> Nothing
  TLet {} -> Nothing
  TTydef {} -> Nothing
  TBind {} -> Nothing
  TDelay {} -> Nothing
  TProj {} -> Nothing
  TAnnotate {} -> Nothing
  TSuspend {} -> Nothing

-- TODO(#2232): in order to get `read` to work for delay, function,
-- and/or command types, we will need to handle a few more of the
-- above cases, e.g. TConst, TLam, TApp, TLet, TBind, TDelay.

negateInt :: Value -> Maybe Value
negateInt = \case
  VInt n -> Just (VInt (-n))
  _ -> Nothing
