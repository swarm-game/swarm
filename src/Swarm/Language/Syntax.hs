{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for terms of the Swarm programming language.
module Swarm.Language.Syntax (
  -- * Directions
  Direction (..),
  AbsoluteDir (..),
  RelativeDir (..),
  directionSyntax,
  isCardinal,
  allDirs,

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
  isOperator,
  isBuiltinFunction,
  isTangible,
  isLong,
  maxSniffRange,
  maxScoutRange,
  maxStrideRange,

  -- * Syntax
  Syntax' (..),
  sLoc,
  sTerm,
  sType,
  Syntax,
  pattern Syntax,
  LocVar (..),
  SrcLoc (..),
  noLoc,
  pattern STerm,
  pattern TRequirements,
  pattern TPair,
  pattern TLam,
  pattern TApp,
  pattern (:$:),
  pattern TLet,
  pattern TDef,
  pattern TBind,
  pattern TDelay,
  pattern TRcd,
  pattern TProj,
  pattern TAnnotate,

  -- * Terms
  Var,
  DelayType (..),
  Term' (..),
  Term,
  mkOp,
  mkOp',
  unfoldApps,

  -- * Erasure
  erase,
  eraseS,

  -- * Term traversal
  freeVarsS,
  freeVarsT,
  freeVarsV,
  mapFreeS,
  locVarToSyntax',
  asTree,
  measureAstSize,
) where

import Control.Lens (Plated (..), Traversal', makeLenses, para, universe, (%~), (^.))
import Data.Aeson.Types hiding (Key)
import Data.Char qualified as C (toLower)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List qualified as L (tail)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Set qualified as S
import Data.String (IsString (fromString))
import Data.Text hiding (filter, length, map)
import Data.Text qualified as T
import Data.Tree
import GHC.Generics (Generic)
import Swarm.Language.Types
import Swarm.Util qualified as Util
import Witch.From (from)

-- | Maximum perception distance for
-- 'chirp' and 'sniff' commands
maxSniffRange :: Int32
maxSniffRange = 256

maxScoutRange :: Int
maxScoutRange = 64

maxStrideRange :: Int
maxStrideRange = 64

------------------------------------------------------------
-- Directions
------------------------------------------------------------

-- | An absolute direction is one which is defined with respect to an
--   external frame of reference; robots need a compass in order to
--   use them.
--
-- NOTE: These values are ordered by increasing angle according to
-- the standard mathematical convention.
-- That is, the right-pointing direction, East, is considered
-- the "reference angle" and the order proceeds counter-clockwise.
-- See https://en.wikipedia.org/wiki/Polar_coordinate_system#Conventions
--
-- Do not alter this ordering, as there exist functions that depend on it
-- (e.g. "nearestDirection" and "relativeTo").
data AbsoluteDir = DEast | DNorth | DWest | DSouth
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, ToJSON, FromJSON, Enum, Bounded)

cardinalDirectionKeyOptions :: JSONKeyOptions
cardinalDirectionKeyOptions =
  defaultJSONKeyOptions
    { keyModifier = map C.toLower . L.tail
    }

instance ToJSONKey AbsoluteDir where
  toJSONKey = genericToJSONKey cardinalDirectionKeyOptions

instance FromJSONKey AbsoluteDir where
  fromJSONKey = genericFromJSONKey cardinalDirectionKeyOptions

-- | A relative direction is one which is defined with respect to the
--   robot's frame of reference; no special capability is needed to
--   use them.
data RelativeDir = DLeft | DRight | DBack | DForward | DDown
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, ToJSON, FromJSON, Enum, Bounded)

-- | The type of directions. Used /e.g./ to indicate which way a robot
--   will turn.
data Direction = DAbsolute AbsoluteDir | DRelative RelativeDir
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, ToJSON, FromJSON)

-- | Direction name is generated from Direction data constuctor
-- e.g. DLeft becomes "left"
directionSyntax :: Direction -> Text
directionSyntax d = toLower . T.tail . from $ case d of
  DAbsolute x -> show x
  DRelative x -> show x

-- | Check if the direction is absolute (e.g. 'north' or 'south').
isCardinal :: Direction -> Bool
isCardinal = \case
  DAbsolute _ -> True
  _ -> False

allDirs :: [Direction]
allDirs = map DAbsolute Util.listEnums <> map DRelative Util.listEnums

------------------------------------------------------------
-- Constants
------------------------------------------------------------

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
--   four, and CI will warn you about the last, so in theory it's not
--   really possible to forget.  Note you do not need to update the
--   parser or pretty-printer, since they are auto-generated from
--   'constInfo'.
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
  | -- | Push an entity forward one step.
    Push
  | -- | Move forward multiple steps.
    Stride
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
  | -- | Equip a device on oneself.
    Equip
  | -- | Unequip an equipped device, returning to inventory.
    Unequip
  | -- | Make an item.
    Make
  | -- | Sense whether we have a certain item.
    Has
  | -- | Sense whether we have a certain device equipped.
    Equipped
  | -- | Sense how many of a certain item we have.
    Count
  | -- | Act upon an entity.
    Act
  | -- | Drill through an entity.
    Drill
  | -- | Use an entity with another.
    Use
  | -- | Construct a new robot.
    Build
  | -- | Deconstruct an old robot.
    Salvage
  | -- | Reprogram a robot that has executed it's command
    --   with a new command
    Reprogram
  | -- | Emit a message.
    Say
  | -- | Listen for a message from other robots.
    Listen
  | -- | Emit a log message.
    Log
  | -- | View a certain robot.
    View
  | -- | Set what characters are used for display.
    Appear
  | -- | Create an entity out of thin air. Only
    --   available in creative mode.
    Create
  | -- | Tell a robot to halt.
    Halt
  | -- Sensing / generation

    -- | Get current time
    Time
  | -- Detect whether a robot is within line-of-sight in a direction
    Scout
  | -- | Get the current x, y coordinates
    Whereami
  | -- | Locate the closest instance of a given entity within the rectangle
    -- specified by opposite corners, relative to the current location.
    Detect
  | -- | Count the number of a given entity within the rectangle
    -- specified by opposite corners, relative to the current location.
    Resonate
  | -- | Get the distance to the closest instance of the specified entity.
    Sniff
  | -- | Get the direction to the closest instance of the specified entity.
    Chirp
  | -- | Register a location to interrupt a `wait` upon changes
    Watch
  | -- | Register a (remote) location to interrupt a `wait` upon changes
    Surveil
  | -- | Get the current heading.
    Heading
  | -- | See if we can move forward or not.
    Blocked
  | -- | Scan a nearby cell
    Scan
  | -- | Upload knowledge to another robot
    Upload
  | -- | See if a specific entity is here.
    Ishere
  | -- | Check whether the current cell is empty
    Isempty
  | -- | Get a reference to oneself
    Self
  | -- | Get the robot's parent
    Parent
  | -- | Get a reference to the base
    Base
  | -- | Meet a nearby robot
    Meet
  | -- | Meet all nearby robots
    MeetAll
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
  | -- | Count number of characters.
    Chars
  | -- | Split string into two parts.
    Split
  | -- | Get the character at an index.
    CharAt
  | -- | Create a singleton text value with the given character code.
    ToChar
  | -- Function composition with nice operators

    -- | Application operator - helps to avoid parentheses:
    --   @f $ g $ h x  =  f (g (h x))@
    AppF
  | -- Concurrency

    -- | Swap placed entity with one in inventory. Essentially atomic grab and place.
    Swap
  | -- | When executing @atomic c@, a robot will not be interrupted,
    --   that is, no other robots will execute any commands while
    --   the robot is executing @c@.
    Atomic
  | -- | Like @atomic@, but with no restriction on program size.
    Instant
  | -- Keyboard input

    -- | Create `key` values.
    Key
  | -- | Install a new keyboard input handler.
    InstallKeyHandler
  | -- God-like commands that are omnipresent or omniscient.

    -- | Teleport a robot to the given position.
    Teleport
  | -- | Run a command as if you were another robot.
    As
  | -- | Find an actor by name.
    RobotNamed
  | -- | Find an actor by number.
    RobotNumbered
  | -- | Check if an entity is known.
    Knows
  deriving (Eq, Ord, Enum, Bounded, Data, Show, Generic, FromJSON, ToJSON)

allConst :: [Const]
allConst = Util.listEnums

data ConstInfo = ConstInfo
  { syntax :: Text
  , fixity :: Int
  , constMeta :: ConstMeta
  , constDoc :: ConstDoc
  , tangibility :: Tangibility
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

-- | Whether a command is tangible or not.  Tangible commands have
--   some kind of effect on the external world; at most one tangible
--   command can be executed per tick.  Intangible commands are things
--   like sensing commands, or commands that solely modify a robot's
--   internal state; multiple intangible commands may be executed per
--   tick.  In addition, tangible commands can have a 'Length' (either
--   'Short' or 'Long') indicating whether they require only one, or
--   possibly more than one, tick to execute.  Long commands are
--   excluded from @atomic@ blocks to avoid freezing the game.
data Tangibility = Intangible | Tangible Length
  deriving (Eq, Ord, Show, Read)

-- | For convenience, @short = Tangible Short@.
short :: Tangibility
short = Tangible Short

-- | For convenience, @long = Tangible Long@.
long :: Tangibility
long = Tangible Long

-- | The length of a tangible command.  Short commands take exactly
--   one tick to execute.  Long commands may require multiple ticks.
data Length = Short | Long
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | The arity of a constant, /i.e./ how many arguments it expects.
--   The runtime system will collect arguments to a constant (see
--   'Swarm.Language.Value.VCApp') until it has enough, then dispatch
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

-- | Whether the constant is an operator. Useful predicate for documentation.
isOperator :: Const -> Bool
isOperator c = case constMeta $ constInfo c of
  ConstMUnOp {} -> True
  ConstMBinOp {} -> True
  ConstMFunc {} -> False

-- | Whether the constant is a /function/ which is interpreted as soon
--   as it is evaluated, but *not* including operators.
--
-- Note: This is used for documentation purposes and complements 'isCmd'
-- and 'isOperator' in that exactly one will accept a given constant.
isBuiltinFunction :: Const -> Bool
isBuiltinFunction c = case constMeta $ constInfo c of
  ConstMFunc _ cmd -> not cmd
  _ -> False

-- | Whether the constant is a /tangible/ command, that has an
--   external effect on the world.  At most one tangible command may be
--   executed per tick.
isTangible :: Const -> Bool
isTangible c = case tangibility (constInfo c) of
  Tangible {} -> True
  _ -> False

-- | Whether the constant is a /long/ command, that is, a tangible
--   command which could require multiple ticks to execute.  Such
--   commands cannot be allowed in @atomic@ blocks.
isLong :: Const -> Bool
isLong c = case tangibility (constInfo c) of
  Tangible Long -> True
  _ -> False

-- | Information about constants used in parsing and pretty printing.
--
-- It would be more compact to represent the information by testing
-- whether the constants are in certain sets, but using pattern
-- matching gives us warning if we add more constants.
constInfo :: Const -> ConstInfo
constInfo c = case c of
  Wait -> command 0 long "Wait for a number of time steps."
  Noop ->
    command 0 Intangible . doc "Do nothing." $
      [ "This is different than `Wait` in that it does not take up a time step."
      , "It is useful for commands like if, which requires you to provide both branches."
      , "Usually it is automatically inserted where needed, so you do not have to worry about it."
      ]
  Selfdestruct ->
    command 0 short . doc "Self-destruct a robot." $
      [ "Useful to not clutter the world."
      , "This destroys the robot's inventory, so consider `salvage` as an alternative."
      ]
  Move -> command 0 short "Move forward one step."
  Push ->
    command 1 short . doc "Push an entity forward one step." $
      [ "Both entity and robot moves forward one step."
      , "Destination must not contain an entity."
      ]
  Stride ->
    command 1 short . doc "Move forward multiple steps." $
      [ T.unwords ["Has a max range of", T.pack $ show maxStrideRange, "units."]
      ]
  Turn -> command 1 short "Turn in some direction."
  Grab -> command 0 short "Grab an item from the current location."
  Harvest ->
    command 0 short . doc "Harvest an item from the current location." $
      [ "Leaves behind a growing seed if the harvested item is growable."
      , "Otherwise it works exactly like `grab`."
      ]
  Place ->
    command 1 short . doc "Place an item at the current location." $
      ["The current location has to be empty for this to work."]
  Give -> command 2 short "Give an item to another actor nearby."
  Equip -> command 1 short "Equip a device on oneself."
  Unequip -> command 1 short "Unequip an equipped device, returning to inventory."
  Make -> command 1 long "Make an item using a recipe."
  Has -> command 1 Intangible "Sense whether the robot has a given item in its inventory."
  Equipped -> command 1 Intangible "Sense whether the robot has a specific device equipped."
  Count -> command 1 Intangible "Get the count of a given item in a robot's inventory."
  Reprogram ->
    command 2 long . doc "Reprogram another robot with a new command." $
      ["The other robot has to be nearby and idle."]
  Act ->
    command 1 long . doc "Act upon an entity." $
      [ "This command transforms an entity in the world using a device."
      , "It will automatically `use` the appropriate device required by a recipe that takes the target entity as input."
      ]
  Drill ->
    command 1 long . doc "Drill through an entity." $
      [ "Usually you want to `drill forward` when exploring to clear out obstacles."
      , "When you have found a source to drill, you can stand on it and `drill down`."
      , "See what recipes with drill you have available."
      , "The `drill` command may return the name of an entity added to your inventory."
      ]
  Use ->
    command 2 long . doc "Use one entity upon another." $
      [ "Which entities you can `use` with others depends on the available recipes."
      , "The object being used must be a 'required' entity in a recipe."
      ]
  Build ->
    command 1 long . doc "Construct a new robot." $
      [ "You can specify a command for the robot to execute."
      , "If the command requires devices they will be taken from your inventory and "
          <> "equipped on the new robot."
      ]
  Salvage ->
    command 0 long . doc "Deconstruct an old robot." $
      ["Salvaging a robot will give you its inventory, equipped devices and log."]
  Say ->
    command 1 short . doc "Emit a message." $
      [ "The message will be in the robot's log (if it has one) and the global log."
      , "You can view the message that would be picked by `listen` from the global log "
          <> "in the messages panel, along with your own messages and logs."
      , "This means that to see messages from other robots you have to be able to listen for them, "
          <> "so once you have a listening device equipped messages will be added to your log."
      , "In creative mode, there is of course no such limitation."
      ]
  Listen ->
    command 1 long . doc "Listen for a message from other actors." $
      [ "It will take the first message said by the closest actor."
      , "You do not need to actively listen for the message to be logged though, "
          <> "that is done automatically once you have a listening device equipped."
      , "Note that you can see the messages either in your logger device or the message panel."
      ]
  Log -> command 1 short "Log the string in the robot's logger."
  View ->
    command 1 short . doc "View the given actor." $
      [ "This will recenter the map on the target robot and allow its inventory and logs to be inspected."
      ]
  Appear ->
    command 1 short . doc "Set how the robot is displayed." $
      [ "You can either specify one character or five (for each direction)."
      , "The default is \"X^>v<\"."
      ]
  Create ->
    command 1 short . doc "Create an item out of thin air." $
      ["Only available in creative mode."]
  Halt -> command 1 short "Tell a robot to halt."
  Time -> command 0 Intangible "Get the current time."
  Scout ->
    command 1 short . doc "Detect whether a robot is within line-of-sight in a direction." $
      [ "Perception is blocked by 'Opaque' entities."
      , T.unwords ["Has a max range of", T.pack $ show maxScoutRange, "units."]
      ]
  Whereami -> command 0 Intangible "Get the current x and y coordinates."
  Detect ->
    command 2 Intangible . doc "Detect an entity within a rectangle." $
      ["Locate the closest instance of a given entity within the rectangle specified by opposite corners, relative to the current location."]
  Resonate ->
    command 2 Intangible . doc "Count entities within a rectangle." $
      [ "Applies a strong magnetic field over a given area and stimulates the matter within, generating a non-directional radio signal. A receiver tuned to the resonant frequency of the target entity is able to measure its quantity."
      , "Counts the entities within the rectangle specified by opposite corners, relative to the current location."
      ]
  Sniff ->
    command 1 short . doc "Determine distance to entity." $
      [ "Measures concentration of airborne particles to infer distance to a certain kind of entity."
      , "If none is detected, returns (-1)."
      , T.unwords ["Has a max range of", T.pack $ show maxSniffRange, "units."]
      ]
  Chirp ->
    command 1 short . doc "Determine direction to entity." $
      [ "Uses a directional sonic emitter and microphone tuned to the acoustic signature of a specific entity to determine its direction."
      , "Returns 'down' if out of range or the direction is indeterminate."
      , "Provides absolute directions if \"compass\" equipped, relative directions otherwise."
      , T.unwords ["Has a max range of", T.pack $ show maxSniffRange, "units."]
      ]
  Watch ->
    command 1 short . doc "Interrupt `wait` upon location changes." $
      [ "Place seismic detectors to alert upon entity changes to the specified location."
      , "Supply a direction, as with the `scan` command, to specify a nearby location."
      , "Can be invoked more than once until the next `wait` command, at which time the only the registered locations that are currently nearby are preserved."
      , "Any change to entities at the monitored locations will cause the robot to wake up before the `wait` timeout."
      ]
  Surveil ->
    command 1 short . doc "Interrupt `wait` upon (remote) location changes." $
      [ "Like `watch`, but with no restriction on distance."
      ]
  Heading -> command 0 Intangible "Get the current heading."
  Blocked -> command 0 Intangible "See if the robot can move forward."
  Scan ->
    command 1 Intangible . doc "Scan a nearby location for entities." $
      [ "Adds the entity (not actor) to your inventory with count 0 if there is any."
      , "If you can use sum types, you can also inspect the result directly."
      ]
  Upload -> command 1 short "Upload a robot's known entities and log to another robot."
  Ishere -> command 1 Intangible "See if a specific entity is in the current location."
  Isempty ->
    command 0 Intangible . doc "Check if the current location is empty." $
      [ "Detects whether or not the current location contains an entity."
      , "Does not detect robots or other actors."
      ]
  Self -> function 0 "Get a reference to the current robot."
  Parent -> function 0 "Get a reference to the robot's parent."
  Base -> function 0 "Get a reference to the base."
  Meet -> command 0 Intangible "Get a reference to a nearby actor, if there is one."
  MeetAll -> command 0 long "Run a command for each nearby actor."
  Whoami -> command 0 Intangible "Get the robot's display name."
  Setname -> command 1 short "Set the robot's display name."
  Random ->
    command 1 Intangible . doc "Get a uniformly random integer." $
      ["The random integer will be chosen from the range 0 to n-1, exclusive of the argument."]
  Run -> command 1 long "Run a program loaded from a file."
  Return -> command 1 Intangible "Make the value a result in `cmd`."
  Try -> command 2 Intangible "Execute a command, catching errors."
  Undefined -> function 0 "A value of any type, that is evaluated as error."
  Fail -> function 1 "A value of any type, that is evaluated as error with message."
  If ->
    function 3 . doc "If-Then-Else function." $
      ["If the bool predicate is true then evaluate the first expression, otherwise the second."]
  Inl -> function 1 "Put the value into the left component of a sum type."
  Inr -> function 1 "Put the value into the right component of a sum type."
  Case -> function 3 "Evaluate one of the given functions on a value of sum type."
  Fst -> function 1 "Get the first value of a pair."
  Snd -> function 1 "Get the second value of a pair."
  Force -> function 1 "Force the evaluation of a delayed value."
  Not -> function 1 "Negate the boolean value."
  Neg -> unaryOp "-" 7 P "Negate the given integer value."
  Add -> binaryOp "+" 6 L "Add the given integer values."
  And -> binaryOp "&&" 3 R "Logical and (true if both values are true)."
  Or -> binaryOp "||" 2 R "Logical or (true if either value is true)."
  Sub -> binaryOp "-" 6 L "Subtract the given integer values."
  Mul -> binaryOp "*" 7 L "Multiply the given integer values."
  Div -> binaryOp "/" 7 L "Divide the left integer value by the right one, rounding down."
  Exp -> binaryOp "^" 8 R "Raise the left integer value to the power of the right one."
  Eq -> binaryOp "==" 4 N "Check that the left value is equal to the right one."
  Neq -> binaryOp "!=" 4 N "Check that the left value is not equal to the right one."
  Lt -> binaryOp "<" 4 N "Check that the left value is lesser than the right one."
  Gt -> binaryOp ">" 4 N "Check that the left value is greater than the right one."
  Leq -> binaryOp "<=" 4 N "Check that the left value is lesser or equal to the right one."
  Geq -> binaryOp ">=" 4 N "Check that the left value is greater or equal to the right one."
  Format -> function 1 "Turn an arbitrary value into a string."
  Concat -> binaryOp "++" 6 R "Concatenate the given strings."
  Chars -> function 1 "Counts the number of characters in the text."
  Split ->
    function 2 . doc "Split the text into two at given position." $
      [ "To be more specific, the following holds for all `text` values `s1` and `s2`:"
      , "`(s1,s2) == split (chars s1) (s1 ++ s2)`"
      , "So split can be used to undo concatenation if you know the length of the original string."
      ]
  CharAt ->
    function 2 . doc "Get the character at a given index." $
      [ "Gets the character (as an `int` representing a Unicode codepoint) at a specific index in a `text` value.  Valid indices are 0 through `chars t - 1`."
      , "Throws an exception if given an out-of-bounds index."
      ]
  ToChar ->
    function 1 . doc "Create a singleton `text` value from the given character code." $
      [ "That is, `chars (toChar c) == 1` and `charAt 0 (toChar c) == c`."
      ]
  AppF ->
    binaryOp "$" 0 R . doc "Apply the function on the left to the value on the right." $
      [ "This operator is useful to avoid nesting parentheses."
      , "For exaple:"
      , "`f $ g $ h x = f (g (h x))`"
      ]
  Swap ->
    command 1 short . doc "Swap placed entity with one in inventory." $
      [ "This essentially works like atomic grab and place."
      , "Use this to avoid race conditions where more robots grab, scan or place in one location."
      ]
  Atomic ->
    command 1 Intangible . doc "Execute a block of commands atomically." $
      [ "When executing `atomic c`, a robot will not be interrupted, that is, no other robots will execute any commands while the robot is executing @c@."
      ]
  Instant ->
    command 1 Intangible . doc "Execute a block of commands instantly." $
      [ "Like `atomic`, but with no restriction on program size."
      ]
  Key ->
    function 1 . doc "Create a key value from a text description." $
      [ "The key description can optionally start with modifiers like 'C-', 'M-', 'A-', or 'S-', followed by either a regular key, or a special key name like 'Down' or 'End'"
      , "For example, 'M-C-x', 'Down', or 'S-4'."
      , "Which key combinations are actually possible to type may vary by keyboard and terminal program."
      ]
  InstallKeyHandler ->
    command 2 Intangible . doc "Install a keyboard input handler." $
      [ "The first argument is a hint line that will be displayed when the input handler is active."
      , "The second argument is a function to handle keyboard inputs."
      ]
  Teleport -> command 2 short "Teleport a robot to the given location."
  As -> command 2 Intangible "Hypothetically run a command as if you were another robot."
  RobotNamed -> command 1 Intangible "Find an actor by name."
  RobotNumbered -> command 1 Intangible "Find an actor by number."
  Knows -> command 1 Intangible "Check if the robot knows about an entity."
 where
  doc b ls = ConstDoc b (T.unlines ls)
  unaryOp s p side d =
    ConstInfo
      { syntax = s
      , fixity = p
      , constMeta = ConstMUnOp side
      , constDoc = d
      , tangibility = Intangible
      }
  binaryOp s p side d =
    ConstInfo
      { syntax = s
      , fixity = p
      , constMeta = ConstMBinOp side
      , constDoc = d
      , tangibility = Intangible
      }
  command a f d =
    ConstInfo
      { syntax = lowShow c
      , fixity = 11
      , constMeta = ConstMFunc a True
      , constDoc = d
      , tangibility = f
      }
  function a d =
    ConstInfo
      { syntax = lowShow c
      , fixity = 11
      , constMeta = ConstMFunc a False
      , constDoc = d
      , tangibility = Intangible
      }

  lowShow :: Show a => a -> Text
  lowShow a = toLower (from (show a))

------------------------------------------------------------
-- Basic terms
------------------------------------------------------------

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
  deriving (Eq, Show, Data, Generic, FromJSON, ToJSON)

-- | A variable with associated source location, used for variable
--   binding sites. (Variable occurrences are a bare TVar which gets
--   wrapped in a Syntax node, so we don't need LocVar for those.)
data LocVar = LV {lvSrcLoc :: SrcLoc, lvVar :: Var}
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

locVarToSyntax' :: LocVar -> ty -> Syntax' ty
locVarToSyntax' (LV s v) = Syntax' s (TVar v)

-- | Terms of the Swarm language.
data Term' ty
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
  | -- | A text literal.
    TText Text
  | -- | An antiquoted Haskell variable name of type Text.
    TAntiText Text
  | -- | A Boolean literal.
    TBool Bool
  | -- | A robot reference.  These never show up in surface syntax, but are
    --   here so we can factor pretty-printing for Values through
    --   pretty-printing for Terms.
    TRobot Int
  | -- | A memory reference.  These likewise never show up in surface syntax,
    --   but are here to facilitate pretty-printing.
    TRef Int
  | -- | Require a specific device to be installed.
    TRequireDevice Text
  | -- | Require a certain number of an entity.
    TRequire Int Text
  | -- | Primitive command to log requirements of a term.  The Text
    --   field is to store the unaltered original text of the term, for use
    --   in displaying the log message (since once we get to execution time the
    --   original term may have been elaborated, e.g. `force` may have been added
    --   around some variables, etc.)
    SRequirements Text (Syntax' ty)
  | -- | A variable.
    TVar Var
  | -- | A pair.
    SPair (Syntax' ty) (Syntax' ty)
  | -- | A lambda expression, with or without a type annotation on the
    --   binder.
    SLam LocVar (Maybe Type) (Syntax' ty)
  | -- | Function application.
    SApp (Syntax' ty) (Syntax' ty)
  | -- | A (recursive) let expression, with or without a type
    --   annotation on the variable. The @Bool@ indicates whether
    --   it is known to be recursive.
    SLet Bool LocVar (Maybe Polytype) (Syntax' ty) (Syntax' ty)
  | -- | A (recursive) definition command, which binds a variable to a
    --   value in subsequent commands. The @Bool@ indicates whether the
    --   definition is known to be recursive.
    SDef Bool LocVar (Maybe Polytype) (Syntax' ty)
  | -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    SBind (Maybe LocVar) (Syntax' ty) (Syntax' ty)
  | -- | Delay evaluation of a term, written @{...}@.  Swarm is an
    --   eager language, but in some cases (e.g. for @if@ statements
    --   and recursive bindings) we need to delay evaluation.  The
    --   counterpart to @{...}@ is @force@, where @force {t} = t@.
    --   Note that 'Force' is just a constant, whereas 'SDelay' has to
    --   be a special syntactic form so its argument can get special
    --   treatment during evaluation.
    SDelay DelayType (Syntax' ty)
  | -- | Record literals @[x1 = e1, x2 = e2, x3, ...]@ Names @x@
    --   without an accompanying definition are sugar for writing
    --   @x=x@.
    SRcd (Map Var (Maybe (Syntax' ty)))
  | -- | Record projection @e.x@
    SProj (Syntax' ty) Var
  | -- | Annotate a term with a type
    SAnnotate (Syntax' ty) Polytype
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic, FromJSON, ToJSON)

-- The Traversable instance for Term (and for Syntax') is used during
-- typechecking: during intermediate type inference, many of the type
-- annotations placed on AST nodes will have unification variables in
-- them. Once we have finished solving everything we need to do a
-- final traversal over all the types in the AST to substitute away
-- all the unification variables (and generalize, i.e. stick 'forall'
-- on, as appropriate).  See the call to 'mapM' in
-- Swarm.Language.Typecheck.runInfer.

type Term = Term' ()

instance Data ty => Plated (Term' ty) where
  plate = uniplate

------------------------------------------------------------
-- Syntax: annotation on top of Terms with SrcLoc and type
------------------------------------------------------------

-- | The surface syntax for the language, with location and type annotations.
data Syntax' ty = Syntax'
  { _sLoc :: SrcLoc
  , _sTerm :: Term' ty
  , _sType :: ty
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic, FromJSON, ToJSON)

instance Data ty => Plated (Syntax' ty) where
  plate = uniplate

data SrcLoc
  = NoLoc
  | -- | Half-open interval from start (inclusive) to end (exclusive)
    SrcLoc Int Int
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

instance Semigroup SrcLoc where
  NoLoc <> l = l
  l <> NoLoc = l
  SrcLoc s1 e1 <> SrcLoc s2 e2 = SrcLoc (min s1 s2) (max e1 e2)

instance Monoid SrcLoc where
  mempty = NoLoc

------------------------------------------------------------
-- Pattern synonyms for untyped terms
------------------------------------------------------------

type Syntax = Syntax' ()

pattern Syntax :: SrcLoc -> Term -> Syntax
pattern Syntax l t = Syntax' l t ()

{-# COMPLETE Syntax #-}

makeLenses ''Syntax'

noLoc :: Term -> Syntax
noLoc = Syntax mempty

-- | Match an untyped term without its 'SrcLoc'.
pattern STerm :: Term -> Syntax
pattern STerm t <-
  Syntax _ t
  where
    STerm t = Syntax mempty t

pattern TRequirements :: Text -> Term -> Term
pattern TRequirements x t = SRequirements x (STerm t)

-- | Match a TPair without syntax
pattern TPair :: Term -> Term -> Term
pattern TPair t1 t2 = SPair (STerm t1) (STerm t2)

-- | Match a TLam without syntax
pattern TLam :: Var -> Maybe Type -> Term -> Term
pattern TLam v ty t <- SLam (lvVar -> v) ty (STerm t)
  where
    TLam v ty t = SLam (LV NoLoc v) ty (STerm t)

-- | Match a TApp without syntax
pattern TApp :: Term -> Term -> Term
pattern TApp t1 t2 = SApp (STerm t1) (STerm t2)

infixl 0 :$:

-- | Convenient infix pattern synonym for application.
pattern (:$:) :: Term -> Syntax -> Term
pattern (:$:) t1 s2 = SApp (STerm t1) s2

-- | Match a TLet without syntax
pattern TLet :: Bool -> Var -> Maybe Polytype -> Term -> Term -> Term
pattern TLet r v pt t1 t2 <- SLet r (lvVar -> v) pt (STerm t1) (STerm t2)
  where
    TLet r v pt t1 t2 = SLet r (LV NoLoc v) pt (STerm t1) (STerm t2)

-- | Match a TDef without syntax
pattern TDef :: Bool -> Var -> Maybe Polytype -> Term -> Term
pattern TDef r v pt t <- SDef r (lvVar -> v) pt (STerm t)
  where
    TDef r v pt t = SDef r (LV NoLoc v) pt (STerm t)

-- | Match a TBind without syntax
pattern TBind :: Maybe Var -> Term -> Term -> Term
pattern TBind mv t1 t2 <- SBind (fmap lvVar -> mv) (STerm t1) (STerm t2)
  where
    TBind mv t1 t2 = SBind (LV NoLoc <$> mv) (STerm t1) (STerm t2)

-- | Match a TDelay without syntax
pattern TDelay :: DelayType -> Term -> Term
pattern TDelay m t = SDelay m (STerm t)

-- | Match a TRcd without syntax
pattern TRcd :: Map Var (Maybe Term) -> Term
pattern TRcd m <- SRcd ((fmap . fmap) _sTerm -> m)
  where
    TRcd m = SRcd ((fmap . fmap) STerm m)

pattern TProj :: Term -> Var -> Term
pattern TProj t x = SProj (STerm t) x

-- | Match a TAnnotate without syntax
pattern TAnnotate :: Term -> Polytype -> Term
pattern TAnnotate t pt = SAnnotate (STerm t) pt

-- | COMPLETE pragma tells GHC using this set of pattern is complete for Term
{-# COMPLETE TUnit, TConst, TDir, TInt, TAntiInt, TText, TAntiText, TBool, TRequireDevice, TRequire, TRequirements, TVar, TPair, TLam, TApp, TLet, TDef, TBind, TDelay, TRcd, TProj, TAnnotate #-}

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

-- | Make infix operation, discarding any syntax related location
mkOp' :: Const -> Term -> Term -> Term
mkOp' c t1 = TApp (TApp (TConst c) t1)

-- $setup
-- >>> import Control.Lens ((^.))

-- | Turn function application chain into a list.
--
-- >>> syntaxWrap f = fmap (^. sTerm) . f . Syntax NoLoc
-- >>> syntaxWrap unfoldApps (mkOp' Mul (TInt 1) (TInt 2)) -- 1 * 2
-- TConst Mul :| [TInt 1,TInt 2]
unfoldApps :: Syntax' ty -> NonEmpty (Syntax' ty)
unfoldApps trm = NonEmpty.reverse . flip NonEmpty.unfoldr trm $ \case
  Syntax' _ (SApp s1 s2) _ -> (s2, Just s1)
  s -> (s, Nothing)

--------------------------------------------------
-- Erasure

-- | Erase a 'Syntax' tree annotated with @SrcLoc@ and type
--   information to a bare unannotated 'Term'.
eraseS :: Syntax' ty -> Term
eraseS (Syntax' _ t _) = erase t

-- | Erase a type-annotated term to a bare term.
erase :: Term' ty -> Term
erase TUnit = TUnit
erase (TConst c) = TConst c
erase (TDir d) = TDir d
erase (TInt n) = TInt n
erase (TAntiInt v) = TAntiInt v
erase (TText t) = TText t
erase (TAntiText v) = TAntiText v
erase (TBool b) = TBool b
erase (TRobot r) = TRobot r
erase (TRef r) = TRef r
erase (TRequireDevice d) = TRequireDevice d
erase (TRequire n e) = TRequire n e
erase (SRequirements x s) = TRequirements x (eraseS s)
erase (TVar s) = TVar s
erase (SDelay x s) = TDelay x (eraseS s)
erase (SPair s1 s2) = TPair (eraseS s1) (eraseS s2)
erase (SLam x mty body) = TLam (lvVar x) mty (eraseS body)
erase (SApp s1 s2) = TApp (eraseS s1) (eraseS s2)
erase (SLet r x mty s1 s2) = TLet r (lvVar x) mty (eraseS s1) (eraseS s2)
erase (SDef r x mty s) = TDef r (lvVar x) mty (eraseS s)
erase (SBind mx s1 s2) = TBind (lvVar <$> mx) (eraseS s1) (eraseS s2)
erase (SRcd m) = TRcd ((fmap . fmap) eraseS m)
erase (SProj s x) = TProj (eraseS s) x
erase (SAnnotate s pty) = TAnnotate (eraseS s) pty

------------------------------------------------------------
-- Free variable traversals
------------------------------------------------------------

-- | Traversal over those subterms of a term which represent free
--   variables.  The S suffix indicates that it is a `Traversal' over
--   the `Syntax` nodes (which contain type and source location info)
--   containing free variables inside a larger `Syntax` value.  Note
--   that if you want to get the list of all `Syntax` nodes
--   representing free variables, you can do so via @'toListOf'
--   'freeVarsS'@.
freeVarsS :: forall ty. Traversal' (Syntax' ty) (Syntax' ty)
freeVarsS f = go S.empty
 where
  -- go :: Applicative f => Set Var -> Syntax' ty -> f (Syntax' ty)
  go bound s@(Syntax' l t ty) = case t of
    TUnit -> pure s
    TConst {} -> pure s
    TDir {} -> pure s
    TInt {} -> pure s
    TAntiInt {} -> pure s
    TText {} -> pure s
    TAntiText {} -> pure s
    TBool {} -> pure s
    TRobot {} -> pure s
    TRef {} -> pure s
    TRequireDevice {} -> pure s
    TRequire {} -> pure s
    SRequirements x s1 -> rewrap $ SRequirements x <$> go bound s1
    TVar x
      | x `S.member` bound -> pure s
      | otherwise -> f s
    SLam x xty s1 -> rewrap $ SLam x xty <$> go (S.insert (lvVar x) bound) s1
    SApp s1 s2 -> rewrap $ SApp <$> go bound s1 <*> go bound s2
    SLet r x xty s1 s2 ->
      let bound' = S.insert (lvVar x) bound
       in rewrap $ SLet r x xty <$> go bound' s1 <*> go bound' s2
    SPair s1 s2 -> rewrap $ SPair <$> go bound s1 <*> go bound s2
    SDef r x xty s1 -> rewrap $ SDef r x xty <$> go (S.insert (lvVar x) bound) s1
    SBind mx s1 s2 -> rewrap $ SBind mx <$> go bound s1 <*> go (maybe id (S.insert . lvVar) mx bound) s2
    SDelay m s1 -> rewrap $ SDelay m <$> go bound s1
    SRcd m -> rewrap $ SRcd <$> (traverse . traverse) (go bound) m
    SProj s1 x -> rewrap $ SProj <$> go bound s1 <*> pure x
    SAnnotate s1 pty -> rewrap $ SAnnotate <$> go bound s1 <*> pure pty
   where
    rewrap s' = Syntax' l <$> s' <*> pure ty

-- | Like 'freeVarsS', but traverse over the 'Term's containing free
--   variables.  More direct if you don't need to know the types or
--   source locations of the variables.  Note that if you want to get
--   the list of all `Term`s representing free variables, you can do so via
--   @'toListOf' 'freeVarsT'@.
freeVarsT :: forall ty. Traversal' (Syntax' ty) (Term' ty)
freeVarsT = freeVarsS . sTerm

-- | Traversal over the free variables of a term.  Like 'freeVarsS'
--   and 'freeVarsT', but traverse over the variable names
--   themselves.  Note that if you want to get the set of all free
--   variable names, you can do so via @'Data.Set.Lens.setOf'
--   'freeVarsV'@.
freeVarsV :: Traversal' (Syntax' ty) Var
freeVarsV = freeVarsT . (\f -> \case TVar x -> TVar <$> f x; t -> pure t)

-- | Apply a function to all free occurrences of a particular variable.
mapFreeS :: Var -> (Syntax' ty -> Syntax' ty) -> Syntax' ty -> Syntax' ty
mapFreeS x f = freeVarsS %~ (\t -> case t ^. sTerm of TVar y | y == x -> f t; _ -> t)

-- | Transform the AST into a Tree datatype.
-- Useful for pretty-printing (e.g. via "Data.Tree.drawTree").
asTree :: Data a => Syntax' a -> Tree (Syntax' a)
asTree = para Node

-- | Each constructor is a assigned a value of 1, plus
-- any recursive syntax it entails.
measureAstSize :: Data a => Syntax' a -> Int
measureAstSize = length . universe
