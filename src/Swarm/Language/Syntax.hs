{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Swarm.Language.Syntax
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for terms of the Swarm programming language.
module Swarm.Language.Syntax (
  -- * Directions
  Direction (..),
  DirInfo (..),
  applyTurn,
  toDirection,
  fromDirection,
  allDirs,
  isCardinal,
  dirInfo,
  north,
  south,
  east,
  west,

  -- * Constants
  Const (..),
  allConst,
  ConstInfo (..),
  ConstDoc (..),
  ConstMeta (..),
  MBinAssoc (..),
  MUnAssoc (..),
  constInfo,
  arity,
  isCmd,
  isUserFunc,

  -- * Syntax
  Syntax (..),
  Location (..),
  noLoc,
  pattern STerm,
  pattern TPair,
  pattern TLam,
  pattern TApp,
  pattern TLet,
  pattern TDef,
  pattern TBind,
  pattern TDelay,

  -- * Terms
  Var,
  DelayType (..),
  Term (..),
  mkOp,
  mkOp',

  -- * Term traversal
  fvT,
  fv,
  mapFree1,
) where

import Control.Lens (Plated (..), Traversal', (%~))
import Data.Aeson.Types
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as S
import Data.String (IsString (fromString))
import Data.Text hiding (filter, map)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Linear
import Swarm.Language.Types
import Witch.From (from)

------------------------------------------------------------
-- Constants
------------------------------------------------------------

-- | The type of directions. Used /e.g./ to indicate which way a robot
--   will turn.
data Direction = DLeft | DRight | DBack | DForward | DNorth | DSouth | DEast | DWest | DDown
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, ToJSON, FromJSON, Enum, Bounded)

instance ToJSONKey Direction where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Direction where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

data DirInfo = DirInfo
  { dirSyntax :: Text
  , -- absolute direction if it exists
    dirAbs :: Maybe (V2 Int64)
  , -- the turning for the direction
    dirApplyTurn :: V2 Int64 -> V2 Int64
  }

allDirs :: [Direction]
allDirs = [minBound .. maxBound]

-- | Information about all directions
dirInfo :: Direction -> DirInfo
dirInfo d = case d of
  DLeft -> relative (\(V2 x y) -> V2 (- y) x)
  DRight -> relative (\(V2 x y) -> V2 y (- x))
  DBack -> relative (\(V2 x y) -> V2 (- x) (- y))
  DDown -> relative (const down)
  DForward -> relative id
  DNorth -> cardinal north
  DSouth -> cardinal south
  DEast -> cardinal east
  DWest -> cardinal west
 where
  -- name is generate from Direction data constuctor
  -- e.g. DLeft becomes "left"
  directionSyntax = toLower . T.tail . from . show $ d
  cardinal v2 = DirInfo directionSyntax (Just v2) (const v2)
  relative = DirInfo directionSyntax Nothing

-- | Check if the direction is absolute (e.g. 'north' or 'south').
isCardinal :: Direction -> Bool
isCardinal = isJust . dirAbs . dirInfo

-- | The cardinal direction north = @V2 0 1@.
north :: V2 Int64
north = V2 0 1

-- | The cardinal direction south = @V2 0 (-1)@.
south :: V2 Int64
south = V2 0 (-1)

-- | The cardinal direction east = @V2 1 0@.
east :: V2 Int64
east = V2 1 0

-- | The cardinal direction west = @V2 (-1) 0@.
west :: V2 Int64
west = V2 (-1) 0

-- | The direction for viewing the current cell = @V2 0 0@.
down :: V2 Int64
down = V2 0 0

-- | The 'applyTurn' function gives the meaning of each 'Direction' by
--   turning relative to the given vector or by turning to an absolute
--   direction vector.
applyTurn :: Direction -> V2 Int64 -> V2 Int64
applyTurn = dirApplyTurn . dirInfo

-- | Mapping from heading to their corresponding cardinal directions
--   only directions with a 'dirAbs` value are mapped
cardinalDirs :: M.Map (V2 Int64) Direction
cardinalDirs =
  M.fromList
    . mapMaybe (\d -> (,d) <$> (dirAbs . dirInfo $ d))
    $ allDirs

-- | Possibly convert a vector into a 'Direction'---that is, if the
--   vector happens to be a unit vector in one of the cardinal
--   directions.
toDirection :: V2 Int64 -> Maybe Direction
toDirection v = M.lookup v cardinalDirs

-- | Convert a 'Direction' into a corresponding vector.  Note that
--   this only does something reasonable for 'DNorth', 'DSouth', 'DEast',
--   and 'DWest'---other 'Direction's return the zero vector.
fromDirection :: Direction -> V2 Int64
fromDirection = fromMaybe (V2 0 0) . dirAbs . dirInfo

-- | Constants, representing various built-in functions and commands.
--
--   IF YOU ADD A NEW CONSTANT, be sure to also update:
--   1. the 'constInfo' function (below)
--   2. the capability checker ("Swarm.Language.Capability")
--   3. the type checker ("Swarm.Language.Typecheck")
--   4. the runtime ("Swarm.Game.Step")
--   5. the emacs mode syntax highlighter (@contribs/swarm-mode.el@)
--
--   GHC will warn you about incomplete pattern matches for the first
--   four, so it's not really possible to forget.  Note you do not
--   need to update the parser or pretty-printer, since they are
--   auto-generated from 'constInfo'.
data Const
  = -- Trivial actions

    -- | Do nothing.  This is different than 'Wait'
    --   in that it does not take up a time step.
    Noop
  | -- | Wait for a number of time steps without doing anything.
    Wait
  | -- | Self-destruct.
    Selfdestruct
  | -- Basic actions

    -- | Move forward one step.
    Move
  | -- | Turn in some direction.
    Turn
  | -- | Grab an item from the current location.
    Grab
  | -- | Harvest an item from the current location.
    Harvest
  | -- | Try to place an item at the current location.
    Place
  | -- | Give an item to another robot at the current location.
    Give
  | -- | Install a device on a robot.
    Install
  | -- | Make an item.
    Make
  | -- | Sense whether we have a certain item.
    Has
  | -- | Sense how many of a certain item we have.
    Count
  | -- | Drill through an entity.
    Drill
  | -- | Construct a new robot.
    Build
  | -- | Deconstruct an old robot.
    Salvage
  | -- | Reprogram a robot that has executed it's command
    --   with a new command
    Reprogram
  | -- | Emit a message.
    Say
  | -- | Emit a log message.
    Log
  | -- | View a certain robot.
    View
  | -- | Set what characters are used for display.
    Appear
  | -- | Create an entity out of thin air. Only
    --   available in creative mode.
    Create
  | -- Sensing / generation

    -- | Get the current x, y coordinates
    Whereami
  | -- | See if we can move forward or not.
    Blocked
  | -- | Scan a nearby cell
    Scan
  | -- | Upload knowledge to another robot
    Upload
  | -- | See if a specific entity is here. (This may be removed.)
    Ishere
  | -- | Get a reference to oneself
    Self
  | -- | Get the robot's parent
    Parent
  | -- | Get a reference to the base
    Base
  | -- | Get the robot's display name
    Whoami
  | -- | Set the robot's display name
    Setname
  | -- | Get a uniformly random integer.
    Random
  | -- Modules

    -- | Run a program loaded from a file.
    Run
  | -- Language built-ins

    -- | If-expressions.
    If
  | -- | Left injection.
    Inl
  | -- | Right injection.
    Inr
  | -- | Case analysis on a sum type.
    Case
  | -- | First projection.
    Fst
  | -- | Second projection.
    Snd
  | -- | Force a delayed evaluation.
    Force
  | -- | Return for the cmd monad.
    Return
  | -- | Try/catch block
    Try
  | -- | Undefined
    Undefined
  | -- | User error
    Fail
  | -- Arithmetic unary operators

    -- | Logical negation.
    Not
  | -- | Arithmetic negation.
    Neg
  | -- Comparison operators

    -- | Logical equality comparison
    Eq
  | -- | Logical unequality comparison
    Neq
  | -- | Logical lesser-then comparison
    Lt
  | -- | Logical greater-then comparison
    Gt
  | -- | Logical lesser-or-equal comparison
    Leq
  | -- | Logical greater-or-equal comparison
    Geq
  | -- Arithmetic binary operators

    -- | Logical or.
    Or
  | -- | Logical and.
    And
  | -- | Arithmetic addition operator
    Add
  | -- | Arithmetic subtraction operator
    Sub
  | -- | Arithmetic multiplication operator
    Mul
  | -- | Arithmetic division operator
    Div
  | -- | Arithmetic exponentiation operator
    Exp
  | -- String operators

    -- | Turn an arbitrary value into a string
    Format
  | -- | Concatenate string values
    Concat
  | -- Function composition with nice operators

    -- | Application operator - helps to avoid parentheses:
    --   @f $ g $ h x  =  f (g (h x))@
    AppF
  | -- God-like commands that are omnipresent or omniscient.

    -- | Teleport a robot to the given position.
    Teleport
  | -- | Run a command as if you were another robot.
    As
  | -- | Find a robot by name.
    RobotNamed
  | -- | Find a robot by number.
    RobotNumbered
  | -- | Check if an entity is known.
    Knows
  deriving (Eq, Ord, Enum, Bounded, Data, Show)

allConst :: [Const]
allConst = [minBound .. maxBound]

data ConstInfo = ConstInfo
  { syntax :: Text
  , fixity :: Int
  , constMeta :: ConstMeta
  , constDoc :: ConstDoc
  }
  deriving (Eq, Ord, Show)

data ConstDoc = ConstDoc {briefDoc :: Text, longDoc :: Text}
  deriving (Eq, Ord, Show)

instance IsString ConstDoc where
  fromString = flip ConstDoc "" . T.pack

data ConstMeta
  = -- | Function with arity of which some are commands
    ConstMFunc Int Bool
  | -- | Unary operator with fixity and associativity.
    ConstMUnOp MUnAssoc
  | -- | Binary operator with fixity and associativity.
    ConstMBinOp MBinAssoc
  deriving (Eq, Ord, Show)

-- | The meta type representing associativity of binary operator.
data MBinAssoc
  = -- |  Left associative binary operator (see 'Control.Monad.Combinators.Expr.InfixL')
    L
  | -- |   Non-associative binary operator (see 'Control.Monad.Combinators.Expr.InfixN')
    N
  | -- | Right associative binary operator (see 'Control.Monad.Combinators.Expr.InfixR')
    R
  deriving (Eq, Ord, Show)

-- | The meta type representing associativity of unary operator.
data MUnAssoc
  = -- |  Prefix unary operator (see 'Control.Monad.Combinators.Expr.Prefix')
    P
  | -- |  Suffix unary operator (see 'Control.Monad.Combinators.Expr.Suffix')
    S
  deriving (Eq, Ord, Show)

-- | The arity of a constant, /i.e./ how many arguments it expects.
--   The runtime system will collect arguments to a constant (see
--   'Swarm.Game.Value.VCApp') until it has enough, then dispatch
--   the constant's behavior.
arity :: Const -> Int
arity c = case constMeta $ constInfo c of
  ConstMUnOp {} -> 1
  ConstMBinOp {} -> 2
  ConstMFunc a _ -> a

-- | Whether a constant represents a /command/.  Constants which are
--   not commands are /functions/ which are interpreted as soon as
--   they are evaluated.  Commands, on the other hand, are not
--   interpreted until being /executed/, that is, when meeting an
--   'FExec' frame.  When evaluated, commands simply turn into a
--   'VCApp'.
isCmd :: Const -> Bool
isCmd c = case constMeta $ constInfo c of
  ConstMFunc _ cmd -> cmd
  _ -> False

-- | Function constants user can call with reserved words ('wait',...).
isUserFunc :: Const -> Bool
isUserFunc c = case constMeta $ constInfo c of
  ConstMFunc {} -> True
  _ -> False

-- | Information about constants used in parsing and pretty printing.
--
-- It would be more compact to represent the information by testing
-- whether the constants are in certain sets, but using pattern
-- matching gives us warning if we add more constants.
constInfo :: Const -> ConstInfo
constInfo c = case c of
  Wait -> commandLow 0 "Wait for a number of time steps."
  Noop ->
    commandLow 0 . doc "Do nothing." $
      [ "This is different than 'Wait' in that it does not take up a time step."
      , "It is useful for commands like if, which requires you to provide both branches."
      , "Usually it is automatically inserted where needed, so you do not have to worry about it."
      ]
  Selfdestruct ->
    commandLow 0 . doc "Self-destruct the robot." $
      [ "Useful to not clutter the world."
      , "This destroys the robots inventory, so consider 'salvage' as an alternative."
      ]
  Move -> commandLow 0 "Move forward one step."
  Turn -> commandLow 1 "Turn in some direction."
  Grab -> commandLow 0 "Grab an item from the current location."
  Harvest ->
    commandLow 0 . doc "Harvest an item from the current location." $
      [ "Leaves behind a growing seed if the harvested item is growable."
      , "Otherwise it works exactly like 'grab'"
      ]
  Place ->
    commandLow 1 . doc "Place an item at the current location." $
      ["The current location has to be empty for this to work."]
  Give -> commandLow 2 "Give an item to another robot closeby."
  Install -> commandLow 2 "Install a device from inventory on a robot."
  Make -> commandLow 1 "Make an item using a recipe."
  Has -> commandLow 1 "Sense whether the robot has a given item in its inventory."
  Count -> commandLow 1 "Get the count of a given item in robots inventory."
  Reprogram ->
    commandLow 2 . doc "Reprogram another robot with new command." $
      ["The other robot has to be closeby and idle."]
  Drill ->
    commandLow 1 . doc "Drill through an entity." $
      [ "Usually you want to 'drill forward' when exploring to clear out obstacles."
      , "When you have found a source to drill, you can stand on it and 'drill down'."
      , "See what recipes with drill you have available."
      ]
  Build ->
    commandLow 1 . doc "Construct a new robot." $
      [ "You can specify a command for the robot to execute."
      , "If the command requires a device it will be installed from your inventory."
      ]
  Salvage ->
    commandLow 0 . doc "Deconstruct an old robot." $
      ["Salvaging a robot will give you its inventory, installed devices and log."]
  Say ->
    commandLow 1 . doc "Emit a message." $ -- TODO: #513
      [ "The message will be in global log, which you can not currently view."
      , "https://github.com/swarm-game/swarm/issues/513"
      ]
  Log -> commandLow 1 "Log the string in the robots logger."
  View -> commandLow 1 "View the given robot."
  Appear ->
    commandLow 1 . doc "Set how the robot is displayed." $
      [ "You can either specify one character or five (for each direction)."
      , "The default is \"X^>v<\"."
      ]
  Create ->
    commandLow 1 . doc "Create an item out of thin air." $
      ["Only available in creative mode."]
  Whereami -> commandLow 0 "Get the current x, y coordinates."
  Blocked -> commandLow 0 "See if the robot can move forward."
  Scan ->
    commandLow 0 . doc "Scan a nearby location for entities." $
      [ "Adds the entity (not robot) to your inventory with count 0 if there is any."
      , "If you can use sum types, you can also inspect the result directly."
      ]
  Upload -> commandLow 1 "Upload robots known entities to another robot."
  Ishere -> commandLow 1 "See if a specific entity is in current location."
  Self -> functionLow 0 "Get a reference to current robot."
  Parent -> functionLow 0 "Get a reference to the robot's parent."
  Base -> functionLow 0 "Get a reference to the base robot."
  Whoami -> commandLow 0 "Get the robot's display name."
  Setname -> commandLow 1 "Set the robot's display name."
  Random ->
    commandLow 1 . doc "Get a uniformly random integer." $
      ["The random integer will be from the range 0 to END inclusive of your choice."]
  Run -> commandLow 1 "Run a program loaded from a file."
  Return -> commandLow 1 "Make the value a result in 'cmd'."
  Try -> commandLow 2 "Execute a command, catching errors."
  Undefined -> functionLow 0 "A value of any type, that is evaluated as error."
  Fail -> functionLow 1 "A value of any type, that is evaluated as error with message."
  If ->
    functionLow 3 . doc "If-Then-Else function." $
      ["If the bool predicate is true then evaluate the first expression, otherwise the second."]
  Inl -> functionLow 1 "Put the value into the left component of a sum type."
  Inr -> functionLow 1 "Put the value into the right component of a sum type."
  Case -> functionLow 3 "Evaluate one of the given functions on a value of sum type."
  Fst -> functionLow 1 "Get the first value of a pair."
  Snd -> functionLow 1 "Get the second value of a pair."
  Force -> functionLow 1 "Force the evaluation of a delayed evaluation value."
  Not -> functionLow 1 "Negate the boolean value."
  Neg -> unaryOp "-" 7 P "Subtract the given integer value."
  Add -> binaryOp "+" 6 L "Add the given integer values."
  And -> binaryOp "&&" 3 R "Logical and (true if both values are true)."
  Or -> binaryOp "||" 2 R "Logical or (true if either value is true)."
  Sub -> binaryOp "-" 6 L "Add the given integer values."
  Mul -> binaryOp "*" 7 L "Multiply the given integer values."
  Div -> binaryOp "/" 7 L "Divide the left integer value by the right one."
  Exp -> binaryOp "^" 8 R "Raise the left integer value to the power of the right one."
  Eq -> binaryOp "==" 4 N "Check that the left value is equal to the right one."
  Neq -> binaryOp "!=" 4 N "Check that the left value is not equal to the right one."
  Lt -> binaryOp "<" 4 N "Check that the left value is lesser than the right one."
  Gt -> binaryOp ">" 4 N "Check that the left value is greater than the right one."
  Leq -> binaryOp "<=" 4 N "Check that the left value is lesser or equal to the right one."
  Geq -> binaryOp ">=" 4 N "Check that the left value is greater or equal to the right one."
  Format -> functionLow 1 "Turn an arbitrary value into a string."
  Concat -> binaryOp "++" 6 R "Concatenate the given strings."
  AppF ->
    binaryOp "$" 0 R . doc "Apply the function on the left to the value on the right." $
      [ "This operator is useful to avoid nesting parentheses."
      , "For exaple:"
      , "'f $ g $ h x = f (g (h x))'"
      ]
  Teleport -> commandLow 2 "Teleport a robot to the given location."
  As -> commandLow 2 "Hypothetically run a command as if you were another robot."
  RobotNamed -> commandLow 1 "Find a robot by name."
  RobotNumbered -> commandLow 1 "Find a robot by number."
  Knows -> commandLow 1 "Check if the robot knows an entity."
 where
  doc b ls = ConstDoc b (T.unlines ls)
  unaryOp s p side d = ConstInfo {syntax = s, fixity = p, constMeta = ConstMUnOp side, constDoc = d}
  binaryOp s p side d = ConstInfo {syntax = s, fixity = p, constMeta = ConstMBinOp side, constDoc = d}
  command s a d = ConstInfo {syntax = s, fixity = 11, constMeta = ConstMFunc a True, constDoc = d}
  function s a d = ConstInfo {syntax = s, fixity = 11, constMeta = ConstMFunc a False, constDoc = d}
  -- takes the number of arguments for a commmand
  commandLow = command (lowShow c)
  functionLow = function (lowShow c)

-- | Make infix operation, discarding any syntax related location
mkOp' :: Const -> Term -> Term -> Term
mkOp' c t1 = TApp (TApp (TConst c) t1)

-- | Make infix operation (e.g. @2 + 3@) a curried function
--   application (@((+) 2) 3@).
mkOp :: Const -> Syntax -> Syntax -> Syntax
mkOp c s1@(Syntax l1 _) s2@(Syntax l2 _) = Syntax newLoc newTerm
 where
  -- The new syntax span both terms
  newLoc = l1 <> l2
  -- We don't assign a source location for the operator since it is
  -- usually provided as-is and it is not likely to be useful.
  sop = noLoc (TConst c)
  newTerm = SApp (Syntax l1 $ SApp sop s1) s2

-- | The surface syntax for the language
data Syntax = Syntax {sLoc :: Location, sTerm :: Term}
  deriving (Eq, Show, Data)

data Location = NoLoc | Location Int Int
  deriving (Eq, Show, Data)

instance Semigroup Location where
  NoLoc <> l = l
  l <> NoLoc = l
  Location s1 e1 <> Location s2 e2 = Location (min s1 s2) (max e1 e2)

instance Monoid Location where
  mempty = NoLoc

noLoc :: Term -> Syntax
noLoc = Syntax mempty

-- | Match a term without its a syntax
pattern STerm :: Term -> Syntax
pattern STerm t <-
  Syntax _ t
  where
    STerm t = Syntax mempty t

-- | Match a TPair without syntax
pattern TPair :: Term -> Term -> Term
pattern TPair t1 t2 = SPair (STerm t1) (STerm t2)

-- | Match a TLam without syntax
pattern TLam :: Var -> Maybe Type -> Term -> Term
pattern TLam v ty t = SLam v ty (STerm t)

-- | Match a TApp without syntax
pattern TApp :: Term -> Term -> Term
pattern TApp t1 t2 = SApp (STerm t1) (STerm t2)

-- | Match a TLet without syntax
pattern TLet :: Bool -> Var -> Maybe Polytype -> Term -> Term -> Term
pattern TLet r v pt t1 t2 = SLet r v pt (STerm t1) (STerm t2)

-- | Match a TDef without syntax
pattern TDef :: Bool -> Var -> Maybe Polytype -> Term -> Term
pattern TDef r v pt t = SDef r v pt (STerm t)

-- | Match a TBind without syntax
pattern TBind :: Maybe Var -> Term -> Term -> Term
pattern TBind v t1 t2 = SBind v (STerm t1) (STerm t2)

-- | Match a TDelay without syntax
pattern TDelay :: DelayType -> Term -> Term
pattern TDelay m t = SDelay m (STerm t)

-- | COMPLETE pragma tells GHC using this set of pattern is complete for Term
{-# COMPLETE TUnit, TConst, TDir, TInt, TAntiInt, TString, TAntiString, TBool, TVar, TPair, TLam, TApp, TLet, TDef, TBind, TDelay #-}

------------------------------------------------------------
-- Terms

-- | Different runtime behaviors for delayed expressions.
data DelayType
  = -- | A simple delay, implemented via a (non-memoized) @VDelay@
    --   holding the delayed expression.
    SimpleDelay
  | -- | A memoized delay, implemented by allocating a mutable cell
    --   with the delayed expression and returning a reference to it.
    --   When the @Maybe Var@ is @Just@, a recursive binding of the
    --   variable with a reference to the delayed expression will be
    --   provided while evaluating the delayed expression itself. Note
    --   that there is no surface syntax for binding a variable within
    --   a recursive delayed expression; the only way we can get
    --   @Just@ here is when we automatically generate a delayed
    --   expression while interpreting a recursive @let@ or @def@.
    MemoizedDelay (Maybe Var)
  deriving (Eq, Show, Data)

-- | Terms of the Swarm language.
data Term
  = -- | The unit value.
    TUnit
  | -- | A constant.
    TConst Const
  | -- | A direction literal.
    TDir Direction
  | -- | An integer literal.
    TInt Integer
  | -- | An antiquoted Haskell variable name of type Integer.
    TAntiInt Text
  | -- | A string literal.
    TString Text
  | -- | An antiquoted Haskell variable name of type Text.
    TAntiString Text
  | -- | A Boolean literal.
    TBool Bool
  | -- | A robot value.  These never show up in surface syntax, but are
    --   here so we can factor pretty-printing for Values through
    --   pretty-printing for Terms.
    TRobot Int
  | -- | A memory reference.  These likewise never show up in surface syntax,
    --   but are here to facilitate pretty-printing.
    TRef Int
  | -- | A variable.
    TVar Var
  | -- | A pair.
    SPair Syntax Syntax
  | -- | A lambda expression, with or without a type annotation on the
    --   binder.
    SLam Var (Maybe Type) Syntax
  | -- | Function application.
    SApp Syntax Syntax
  | -- | A (recursive) let expression, with or without a type
    --   annotation on the variable. The @Bool@ indicates whether
    --   it is known to be recursive.
    SLet Bool Var (Maybe Polytype) Syntax Syntax
  | -- | A (recursive) definition command, which binds a variable to a
    --   value in subsequent commands. The @Bool@ indicates whether the
    --   definition is known to be recursive.
    SDef Bool Var (Maybe Polytype) Syntax
  | -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    SBind (Maybe Var) Syntax Syntax
  | -- | Delay evaluation of a term, written @{...}@.  Swarm is an
    --   eager language, but in some cases (e.g. for @if@ statements
    --   and recursive bindings) we need to delay evaluation.  The
    --   counterpart to @{...}@ is @force@, where @force {t} = t@.
    --   Note that 'Force' is just a constant, whereas 'SDelay' has to
    --   be a special syntactic form so its argument can get special
    --   treatment during evaluation.
    SDelay DelayType Syntax
  deriving (Eq, Show, Data)

instance Plated Term where
  plate = uniplate

-- | Traversal over those subterms of a term which represent free
--   variables.
fvT :: Traversal' Term Term
fvT f = go S.empty
 where
  go bound t = case t of
    TUnit -> pure t
    TConst {} -> pure t
    TDir {} -> pure t
    TInt {} -> pure t
    TAntiInt {} -> pure t
    TString {} -> pure t
    TAntiString {} -> pure t
    TBool {} -> pure t
    TRobot {} -> pure t
    TRef {} -> pure t
    TVar x
      | x `S.member` bound -> pure t
      | otherwise -> f (TVar x)
    SLam x ty (Syntax l1 t1) -> SLam x ty <$> (Syntax l1 <$> go (S.insert x bound) t1)
    SApp (Syntax l1 t1) (Syntax l2 t2) ->
      SApp <$> (Syntax l1 <$> go bound t1) <*> (Syntax l2 <$> go bound t2)
    SLet r x ty (Syntax l1 t1) (Syntax l2 t2) ->
      let bound' = S.insert x bound
       in SLet r x ty <$> (Syntax l1 <$> go bound' t1) <*> (Syntax l2 <$> go bound' t2)
    SPair (Syntax l1 t1) (Syntax l2 t2) ->
      SPair <$> (Syntax l1 <$> go bound t1) <*> (Syntax l2 <$> go bound t2)
    SDef r x ty (Syntax l1 t1) ->
      SDef r x ty <$> (Syntax l1 <$> go (S.insert x bound) t1)
    SBind mx (Syntax l1 t1) (Syntax l2 t2) ->
      SBind mx <$> (Syntax l1 <$> go bound t1) <*> (Syntax l2 <$> go (maybe id S.insert mx bound) t2)
    SDelay m (Syntax l1 t1) ->
      SDelay m <$> (Syntax l1 <$> go bound t1)

-- | Traversal over the free variables of a term.  Note that if you
--   want to get the set of all free variables, you can do so via
--   @'Data.Set.Lens.setOf' 'fv'@.
fv :: Traversal' Term Var
fv = fvT . (\f -> \case TVar x -> TVar <$> f x; t -> pure t)

-- | Apply a function to all free occurrences of a particular variable.
mapFree1 :: Var -> (Term -> Term) -> Term -> Term
mapFree1 x f = fvT %~ (\t -> if t == TVar x then f t else t)

lowShow :: Show a => a -> Text
lowShow a = toLower (from (show a))
