{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Implementation of robot commands
module Swarm.Game.Step.Const where

import Control.Applicative (Applicative (..))
import Control.Arrow ((&&&))
import Control.Carrier.State.Lazy
import Control.Effect.Error
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Lens as Lens hiding (Const, distrib, from, parts, use, uses, view, (%=), (+=), (.=), (<+=), (<>=))
import Control.Monad (forM, forM_, guard, msum, unless, when)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Char (chr, ord)
import Data.Either (partitionEithers, rights)
import Data.Foldable (asum, for_, traverse_)
import Data.Foldable.Extra (findM, firstJustM)
import Data.Function (on)
import Data.Functor (void)
import Data.Int (Int32)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List (find, sortOn)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NEM
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Linear (V2 (..), perp, zero)
import Swarm.Effect as Effect (Time, getNow)
import Swarm.Game.Achievement.Definitions
import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Exception
import Swarm.Game.Failure
import Swarm.Game.Location
import Swarm.Game.Recipe
import Swarm.Game.ResourceLoading (getDataFileNameSafe)
import Swarm.Game.Robot
import Swarm.Game.Scenario.Topography.Area (getAreaDimensions)
import Swarm.Game.Scenario.Topography.Navigation.Portal (Navigation (..))
import Swarm.Game.Scenario.Topography.Navigation.Util
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (WaypointName (..))
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure.Recognition (automatons, foundStructures)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (foundByName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.State
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step.Arithmetic
import Swarm.Game.Step.Combustion qualified as Combustion
import Swarm.Game.Step.Path.Finding
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.Path.Walkability
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util
import Swarm.Game.Step.Util.Command
import Swarm.Game.Step.Util.Inspect
import Swarm.Game.Universe
import Swarm.Game.Value
import Swarm.Language.Capability
import Swarm.Language.Context hiding (delete)
import Swarm.Language.Key (parseKeyComboFull)
import Swarm.Language.Parse (runParser)
import Swarm.Language.Pipeline
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Requirement qualified as R
import Swarm.Language.Syntax
import Swarm.Language.Text.Markdown qualified as Markdown
import Swarm.Language.Value
import Swarm.Log
import Swarm.Util hiding (both)
import Swarm.Util.Effect (throwToMaybe)
import Witch (From (from), into)
import Prelude hiding (Applicative (..), lookup)

-- | How to handle failure, for example when moving to blocked location
data RobotFailure = ThrowExn | Destroy | IgnoreFail

-- | How to handle different types of failure when moving/teleporting
--   to a location.
type MoveFailureHandler = MoveFailureMode -> RobotFailure

-- | Interpret the execution (or evaluation) of a constant application
--   to some values.
execConst ::
  (HasRobotStepState sig m, Has Effect.Time sig m, Has (Lift IO) sig m) =>
  -- | Need to pass this function as an argument to avoid module import cycle
  -- The supplied function invokes 'runCESK', which lives in "Swarm.Game.Step".
  (Store -> Robot -> Value -> m Value) ->
  Const ->
  [Value] ->
  Store ->
  Cont ->
  m CESK
execConst runChildProg c vs s k = do
  -- First, ensure the robot is capable of executing/evaluating this constant.
  ensureCanExecute c

  -- Increment command count regardless of success
  when (isTangible c) $
    activityCounts . tangibleCommandCount += 1

  activityCounts . commandsHistogram %= M.insertWith (+) c 1

  -- Now proceed to actually carry out the operation.
  case c of
    Noop -> return $ mkReturn ()
    Return -> case vs of
      [v] -> return $ Out v s k
      _ -> badConst
    Wait -> case vs of
      [VInt d] -> do
        time <- use $ temporal . ticks
        purgeFarAwayWatches
        return $ Waiting (addTicks (fromIntegral d) time) (mkReturn ())
      _ -> badConst
    Selfdestruct -> do
      destroyIfNotBase $ \case False -> Just AttemptSelfDestructBase; _ -> Nothing
      flagRedraw
      return $ mkReturn ()
    Move -> do
      orientation <- use robotOrientation
      moveInDirection $ orientation ? zero
    Backup -> do
      orientation <- use robotOrientation
      moveInDirection $ applyTurn (DRelative $ DPlanar DBack) $ orientation ? zero
    Path -> case vs of
      [VInj hasLimit limitVal, VInj findEntity goalVal] -> do
        maybeLimit <-
          if hasLimit
            then case limitVal of
              VInt d -> return $ Just d
              _ -> badConst
            else return Nothing
        goal <-
          if findEntity
            then case goalVal of
              VText eName -> return $ EntityTarget eName
              _ -> badConst
            else case goalVal of
              VPair (VInt x) (VInt y) ->
                return $
                  LocationTarget $
                    Location (fromIntegral x) (fromIntegral y)
              _ -> badConst
        robotLoc <- use robotLocation
        result <- pathCommand $ PathfindingParameters maybeLimit robotLoc goal
        return $ mkReturn result
      _ -> badConst
    Push -> do
      -- Figure out where we're going
      loc <- use robotLocation
      orientation <- use robotOrientation
      let applyHeading = (`offsetBy` (orientation ? zero))
          nextLoc = applyHeading loc
          placementLoc = applyHeading nextLoc

      -- If unobstructed, the robot will move even if
      -- there is nothing to push.
      maybeCurrentE <- entityAt nextLoc
      case maybeCurrentE of
        Just e -> do
          -- Make sure there's nothing already occupying the destination
          nothingHere <- isNothing <$> entityAt placementLoc
          nothingHere `holdsOrFail` ["Something is in the way!"]

          let verbed = verbedGrabbingCmd Push'
          -- Ensure it can be pushed.
          omni <- isPrivilegedBot
          (omni || e `hasProperty` Portable && not (e `hasProperty` Liquid))
            `holdsOrFail` ["The", e ^. entityName, "here can't be", verbed <> "."]

          -- Place the entity and remove it from previous loc
          updateEntityAt nextLoc (const Nothing)
          updateEntityAt placementLoc (const (Just e))
        Nothing -> return ()

      updateRobotLocation loc nextLoc
      return $ mkReturn ()
    Stride -> case vs of
      [VInt d] -> do
        when (d > fromIntegral maxStrideRange) $
          throwError $
            CmdFailed
              Stride
              ( T.unwords
                  [ "Can only stride up to"
                  , T.pack $ show maxStrideRange
                  , "units."
                  ]
              )
              Nothing

        -- Figure out where we're going
        loc <- use robotLocation
        orientation <- use robotOrientation
        let heading = orientation ? zero

        -- Excludes the base location.
        let locsInDirection :: [Cosmic Location]
            locsInDirection =
              take (min (fromIntegral d) maxStrideRange) $
                drop 1 $
                  iterate (`offsetBy` heading) loc

        failureMaybes <- mapM checkMoveFailure locsInDirection
        let maybeFirstFailure = asum failureMaybes

        applyMoveFailureEffect maybeFirstFailure $ \case
          PathBlocked -> ThrowExn
          PathLiquid -> Destroy

        let maybeLastLoc = do
              guard $ null maybeFirstFailure
              listToMaybe $ reverse locsInDirection

        forM_ maybeLastLoc $ updateRobotLocation loc

        return $ mkReturn ()
      _ -> badConst
    Teleport -> case vs of
      [VRobot rid, VPair (VInt x) (VInt y)] -> do
        -- Make sure the other robot exists and is close
        target <- getRobotWithinTouch rid
        -- either change current robot or one in robot map
        let oldLoc = target ^. robotLocation
            nextLoc = fmap (const $ Location (fromIntegral x) (fromIntegral y)) oldLoc

        onTarget rid $ do
          checkMoveAhead nextLoc $ \case
            PathBlocked -> Destroy
            PathLiquid -> Destroy
          updateRobotLocation oldLoc nextLoc

        return $ mkReturn ()
      _ -> badConst
    Grab -> mkReturn <$> doGrab Grab'
    Harvest -> mkReturn <$> doGrab Harvest'
    Ignite -> case vs of
      [VDir d] -> do
        Combustion.igniteCommand c d
        return $ mkReturn ()
      _ -> badConst
    Swap -> case vs of
      [VText name] -> do
        loc <- use robotLocation
        -- Make sure the robot has the thing in its inventory
        e <- hasInInventoryOrFail name
        -- Grab
        newE <- doGrab Swap'

        -- Place the entity and remove it from the inventory
        updateEntityAt loc (const (Just e))
        robotInventory %= delete e

        when (e == newE) $
          grantAchievement SwapSame

        return $ mkReturn newE
      _ -> badConst
    Turn -> case vs of
      [VDir d] -> do
        when (isCardinal d) $ hasCapabilityFor COrient (TDir d)
        robotOrientation . _Just %= applyTurn d
        flagRedraw

        inst <- use equippedDevices
        when (d == DRelative DDown && countByName "compass" inst == 0) $ do
          grantAchievement GetDisoriented

        return $ mkReturn ()
      _ -> badConst
    Place -> case vs of
      [VText name] -> do
        loc <- use robotLocation

        -- Make sure there's nothing already here
        nothingHere <- isNothing <$> entityAt loc
        nothingHere `holdsOrFail` ["There is already an entity here."]

        -- Make sure the robot has the thing in its inventory
        e <- hasInInventoryOrFail name

        -- Place the entity and remove it from the inventory
        updateEntityAt loc (const (Just e))
        robotInventory %= delete e

        flagRedraw
        return $ mkReturn ()
      _ -> badConst
    Ping -> case vs of
      [VRobot otherID] -> do
        maybeOtherRobot <- robotWithID otherID
        selfRobot <- get
        return $ mkReturn $ displacementVector selfRobot maybeOtherRobot
       where
        displacementVector :: Robot -> Maybe Robot -> Maybe (V2 Int32)
        displacementVector selfRobot maybeOtherRobot = do
          otherRobot <- maybeOtherRobot
          let dist = (cosmoMeasure euclidean `on` view robotLocation) selfRobot otherRobot
              (_minRange, maxRange) = getRadioRange (Just selfRobot) (Just otherRobot)
          d <- getFiniteDistance dist
          guard $ d <= maxRange
          orientationBasedRelativePosition selfRobot $ view robotLocation otherRobot
      _ -> badConst
    Give -> case vs of
      [VRobot otherID, VText itemName] -> do
        -- Make sure the other robot exists and is close
        _other <- getRobotWithinTouch otherID

        item <- ensureItem itemName "give"

        -- Giving something to ourself should be a no-op.  We need
        -- this as a special case since it will not work to modify
        -- ourselves in the robotMap --- after performing a tick we
        -- return a modified Robot which gets put back in the
        -- robotMap, overwriting any changes to this robot made
        -- directly in the robotMap during the tick.
        myID <- use robotID
        focusedID <- use $ robotInfo . focusedRobotID
        if otherID /= myID
          then do
            -- Make the exchange
            robotInfo . robotMap . at otherID . _Just . robotInventory %= insert item
            robotInventory %= delete item

            -- Flag the UI for a redraw if we are currently showing either robot's inventory
            when (focusedID == myID || focusedID == otherID) flagRedraw
          else grantAchievement GaveToSelf

        return $ mkReturn ()
      _ -> badConst
    Equip -> case vs of
      [VText itemName] -> do
        item <- ensureItem itemName "equip"
        myID <- use robotID
        focusedID <- use $ robotInfo . focusedRobotID
        -- Don't do anything if the robot already has the device.
        already <- use (equippedDevices . to (`E.contains` item))
        unless already $ do
          equippedDevices %= insert item
          robotInventory %= delete item

          -- Flag the UI for a redraw if we are currently showing our inventory
          when (focusedID == myID) flagRedraw

        return $ mkReturn ()
      _ -> badConst
    Unequip -> case vs of
      [VText itemName] -> do
        item <- ensureEquipped itemName
        myID <- use robotID
        focusedID <- use $ robotInfo . focusedRobotID
        equippedDevices %= delete item
        robotInventory %= insert item
        -- Flag the UI for a redraw if we are currently showing our inventory
        when (focusedID == myID) flagRedraw
        return $ mkReturn ()
      _ -> badConst
    Make -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        ins <- use equippedDevices
        em <- use $ landscape . entityMap
        e <-
          lookupEntityName name em
            `isJustOrFail` ["I've never heard of", indefiniteQ name <> "."]

        outRs <- use $ recipesInfo . recipesOut

        creative <- use creativeMode
        let create l = l <> ["You can use 'create \"" <> name <> "\"' instead." | creative]

        -- Only consider recipes where the number of things we are trying to make
        -- is greater in the outputs than in the inputs.  This prevents us from doing
        -- silly things like making copper pipes when the user says "make furnace".
        let recipes = filter increase (recipesFor outRs e)
            increase r = countIn (r ^. recipeOutputs) > countIn (r ^. recipeInputs)
            countIn xs = maybe 0 fst (find ((== e) . snd) xs)
        not (null recipes)
          `holdsOrFail` create ["There is no known recipe for making", indefinite name <> "."]

        let displayMissingCount mc = \case
              MissingInput -> from (show mc)
              MissingCatalyst -> "not equipped"
            displayMissingIngredient (MissingIngredient mk mc me) =
              "  - " <> me ^. entityName <> " (" <> displayMissingCount mc mk <> ")"
            displayMissingIngredients xs = L.intercalate ["OR"] (map displayMissingIngredient <$> xs)

        -- Try recipes and make a weighted random choice among the
        -- ones we have ingredients for.
        let (badRecipes, goodRecipes) = partitionEithers . map (make (inv, ins)) $ recipes
        chosenRecipe <- weightedChoice (^. _3 . recipeWeight) goodRecipes
        (invTaken, changeInv, recipe) <-
          chosenRecipe
            `isJustOrFail` create
              [ "You don't have the ingredients to make"
              , indefinite name <> "."
              , "Missing:\n" <> T.unlines (displayMissingIngredients badRecipes)
              ]

        -- take recipe inputs from inventory and add outputs after recipeTime
        robotInventory .= invTaken
        traverse_ (updateDiscoveredEntities . snd) (recipe ^. recipeOutputs)
        finishCookingRecipe recipe VUnit [] (map (uncurry AddEntity) changeInv)
      _ -> badConst
    Has -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        return . mkReturn . (> 0) $ countByName name inv
      _ -> badConst
    Equipped -> case vs of
      [VText name] -> do
        inv <- use equippedDevices
        return . mkReturn . (> 0) $ countByName name inv
      _ -> badConst
    Count -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        return . mkReturn $ countByName name inv
      _ -> badConst
    Scout -> case vs of
      [VDir d] -> do
        rMap <- use $ robotInfo . robotMap
        myLoc <- use robotLocation
        heading <- deriveHeading d
        botsByLocs <- use $ robotInfo . robotsByLocation
        selfRid <- use robotID

        -- Includes the base location, so we exclude the base robot later.
        let locsInDirection :: [Cosmic Location]
            locsInDirection = take maxScoutRange $ iterate (`offsetBy` heading) myLoc

        let hasOpaqueEntity =
              fmap (maybe False (`hasProperty` E.Opaque)) . entityAt

        let hasVisibleBot :: Cosmic Location -> Bool
            hasVisibleBot = any botIsVisible . IS.toList . excludeSelf . botsHere
             where
              excludeSelf = (`IS.difference` IS.singleton selfRid)
              botsHere (Cosmic swName loc) =
                M.findWithDefault mempty loc $
                  M.findWithDefault mempty swName botsByLocs
              botIsVisible = maybe False canSee . (`IM.lookup` rMap)
              canSee = not . (^. robotDisplay . invisible)

        -- A robot on the same cell as an opaque entity is considered hidden.
        -- Returns (Just Bool) if the result is conclusively visible or opaque,
        -- or Nothing if we don't have a conclusive answer yet.
        let isConclusivelyVisible :: Bool -> Cosmic Location -> Maybe Bool
            isConclusivelyVisible isOpaque loc
              | isOpaque = Just False
              | hasVisibleBot loc = Just True
              | otherwise = Nothing

        let isConclusivelyVisibleM loc = do
              opaque <- hasOpaqueEntity loc
              return $ isConclusivelyVisible opaque loc

        -- This ensures that we only evaluate locations until
        -- a conclusive result is obtained, so we don't always
        -- have to inspect the maximum range of the command.
        result <- firstJustM isConclusivelyVisibleM locsInDirection
        let foundBot = fromMaybe False result
        return $ mkReturn foundBot
      _ -> badConst
    Whereami -> do
      loc <- use robotLocation
      return $ mkReturn $ loc ^. planar
    Waypoint -> case vs of
      [VText name, VInt idx] -> do
        lm <- use $ landscape . worldNavigation
        Cosmic swName _ <- use robotLocation
        case M.lookup (WaypointName name) $ M.findWithDefault mempty swName $ waypoints lm of
          Nothing -> throwError $ CmdFailed Waypoint (T.unwords ["No waypoint named", name]) Nothing
          Just wps -> return $ mkReturn (NE.length wps, indexWrapNonEmpty wps idx)
      _ -> badConst
    Structure -> case vs of
      [VText name, VInt idx] -> do
        registry <- use $ discovery . structureRecognition . foundStructures
        let maybeFoundStructures = M.lookup (StructureName name) $ foundByName registry
            mkOutput mapNE = (NE.length xs, bottomLeftCorner)
             where
              xs = NEM.toList mapNE
              (pos, struc) = indexWrapNonEmpty xs idx
              topLeftCorner = pos ^. planar
              offsetHeight = V2 0 $ -fromIntegral (length (entityGrid struc) - 1)
              bottomLeftCorner :: Location
              bottomLeftCorner = topLeftCorner .+^ offsetHeight
        return $ mkReturn $ mkOutput <$> maybeFoundStructures
      _ -> badConst
    Floorplan -> case vs of
      [VText name] -> do
        structureTemplates <- use $ discovery . structureRecognition . automatons . originalStructureDefinitions
        let maybeStructure = M.lookup (StructureName name) structureTemplates
        structureDef <-
          maybeStructure
            `isJustOr` cmdExn Floorplan (pure $ T.unwords ["Unknown structure", quote name])
        return . mkReturn . getAreaDimensions $ entityProcessedGrid structureDef
      _ -> badConst
    HasTag -> case vs of
      [VText eName, VText tName] -> do
        em <- use $ landscape . entityMap
        e <-
          lookupEntityName eName em
            `isJustOrFail` ["I've never heard of", indefiniteQ eName <> "."]
        return $ mkReturn $ tName `S.member` (e ^. entityTags)
      _ -> badConst
    TagMembers -> case vs of
      [VText tagName, VInt idx] -> do
        tm <- use $ discovery . tagMembers
        case M.lookup tagName tm of
          Nothing -> throwError $ CmdFailed TagMembers (T.unwords ["No tag named", tagName]) Nothing
          Just theMembers -> return $ mkReturn (NE.length theMembers, indexWrapNonEmpty theMembers idx)
      _ -> badConst
    Detect -> case vs of
      [VText name, VRect x1 y1 x2 y2] -> do
        loc <- use robotLocation
        let locs = rectCells x1 y1 x2 y2
        -- sort offsets by (Manhattan) distance so that we return the closest occurrence
        let sortedOffsets = sortOn (\(V2 x y) -> abs x + abs y) locs
        let f = fmap (maybe False $ isEntityNamed name) . entityAt . offsetBy loc
        firstOne <- findM f sortedOffsets
        return $ mkReturn firstOne
      _ -> badConst
    Resonate -> case vs of
      [VText name, VRect x1 y1 x2 y2] -> doResonate (maybe False $ isEntityNamed name) x1 y1 x2 y2
      _ -> badConst
    Density -> case vs of
      [VRect x1 y1 x2 y2] -> doResonate isJust x1 y1 x2 y2
      _ -> badConst
    Sniff -> case vs of
      [VText name] -> do
        firstFound <- findNearest name
        return $ mkReturn $ maybe (-1) fst firstFound
      _ -> badConst
    Watch -> case vs of
      [VDir d] -> do
        (loc, _me) <- lookInDirection d
        addWatchedLocation loc
        return $ mkReturn ()
      _ -> badConst
    Surveil -> case vs of
      [VPair (VInt x) (VInt y)] -> do
        Cosmic swName _ <- use robotLocation
        let loc = Cosmic swName $ Location (fromIntegral x) (fromIntegral y)
        addWatchedLocation loc
        return $ mkReturn ()
      _ -> badConst
    Chirp -> case vs of
      [VText name] -> do
        firstFound <- findNearest name
        mh <- use robotOrientation
        inst <- use equippedDevices
        let processDirection entityDir =
              if countByName "compass" inst >= 1
                then Just $ DAbsolute entityDir
                else case mh >>= toDirection of
                  Just (DAbsolute robotDir) ->
                    Just . DRelative . DPlanar $ entityDir `relativeTo` robotDir
                  _ -> Nothing -- This may happen if the robot is facing "down"
            d = fromMaybe (DRelative DDown) $ do
              entLoc <- firstFound
              guard $ snd entLoc /= zero
              processDirection . nearestDirection . snd $ entLoc
        return $ mkReturn d
      _ -> badConst
    Heading -> do
      mh <- use robotOrientation
      -- In general, (1) entities might not have an orientation, and
      -- (2) even if they do, orientation is a general vector, which
      -- might not correspond to a cardinal direction.  We could make
      -- 'heading' return a 'maybe dir' i.e. 'unit + dir', or return a
      -- vector of type 'int * int', but those would both be annoying
      -- for players in the vast majority of cases.  We rather choose
      -- to just return the direction 'down' in any case where we don't
      -- otherwise have anything reasonable to return.
      return . mkReturn . fromMaybe (DRelative DDown) $ mh >>= toDirection
    Time -> do
      TickNumber t <- use $ temporal . ticks
      return $ Out (VInt $ fromIntegral t) s k
    Drill -> case vs of
      [VDir d] -> doDrill d
      _ -> badConst
    Use -> case vs of
      [VText deviceName, VDir d] -> do
        ins <- use equippedDevices
        equippedEntity <- ensureEquipped deviceName
        let verbPhrase = T.unwords ["use", deviceName, "on"]
        applyDevice ins verbPhrase d equippedEntity
      _ -> badConst
    Blocked -> do
      loc <- use robotLocation
      orientation <- use robotOrientation
      let nextLoc = loc `offsetBy` (orientation ? zero)
      me <- entityAt nextLoc
      return $ mkReturn $ maybe False (`hasProperty` Unwalkable) me
    Scan -> case vs of
      [VDir d] -> do
        (_loc, me) <- lookInDirection d
        for_ me $ \e -> do
          robotInventory %= insertCount 0 e
          updateDiscoveredEntities e
          -- Flag the world for a redraw since scanning something may
          -- change the way it is drawn (if the base is doing the
          -- scanning)
          flagRedraw
        return $ mkReturn me
      _ -> badConst
    Knows -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        ins <- use equippedDevices
        let allKnown = inv `E.union` ins
        let knows = case E.lookupByName name allKnown of
              [] -> False
              _ -> True
        return $ mkReturn knows
      _ -> badConst
    Upload -> case vs of
      [VRobot otherID] -> do
        -- Make sure the other robot exists and is close
        _other <- getRobotWithinTouch otherID

        -- Upload knowledge of everything in our inventory
        inv <- use robotInventory
        forM_ (elems inv) $ \(_, e) ->
          robotInfo . robotMap . at otherID . _Just . robotInventory %= insertCount 0 e

        -- Upload our log
        rlog <- use robotLog
        robotInfo . robotMap . at otherID . _Just . robotLog <>= rlog

        -- Flag the world for redraw since uploading may change the
        -- base's knowledge and hence how entities are drawn (if they
        -- go from unknown to known).
        flagRedraw

        return $ mkReturn ()
      _ -> badConst
    Random -> case vs of
      [VInt hi] -> do
        n <- uniform (0, hi - 1)
        return $ mkReturn n
      _ -> badConst
    Atomic -> goAtomic
    Instant -> goAtomic
    As -> case vs of
      [VRobot rid, prog] -> do
        r <- robotWithID rid >>= (`isJustOrFail` ["There is no actor with ID", from (show rid)])
        v <- runChildProg s r prog

        -- Return the value returned by the hypothetical command.
        return $ Out v s k
      _ -> badConst
    RobotNamed -> case vs of
      [VText rname] -> do
        r <- robotWithName rname >>= (`isJustOrFail` ["There is no robot named", rname])
        return $ mkReturn r
      _ -> badConst
    RobotNumbered -> case vs of
      [VInt rid] -> do
        r <-
          robotWithID (fromIntegral rid)
            >>= (`isJustOrFail` ["There is no robot with number", from (show rid)])
        return $ mkReturn r
      _ -> badConst
    Say -> case vs of
      [VText msg] -> do
        isPrivileged <- isPrivilegedBot
        loc <- use robotLocation

        -- current robot will be inserted into the robot set, so it needs the log
        m <- traceLog Said Info msg
        emitMessage m
        let measureToLog robLoc = \case
              RobotLog _ _ logLoc -> cosmoMeasure manhattan robLoc logLoc
              SystemLog -> Measurable 0
            addLatestClosest rl = \case
              Seq.Empty -> Seq.singleton m
              es Seq.:|> e
                | e `isEarlierThan` m -> es |> e |> m
                | e `isFartherThan` m -> es |> m
                | otherwise -> es |> e
             where
              isEarlierThan = (<) `on` (^. leTime)
              isFartherThan = (>) `on` (measureToLog rl . view leSource)
        let addToRobotLog :: (Has (State GameState) sgn m) => Robot -> m ()
            addToRobotLog r = do
              maybeRidLoc <- evalState r $ do
                hasLog <- hasCapability CLog
                hasListen <- hasCapability CListen
                loc' <- use robotLocation
                rid <- use robotID
                return $ do
                  guard $ hasLog && hasListen
                  Just (rid, loc')
              forM_ maybeRidLoc $ \(rid, loc') ->
                robotInfo . robotMap . at rid . _Just . robotLog %= addLatestClosest loc'
        robotsAround <-
          zoomRobots $
            if isPrivileged
              then use $ robotMap . to IM.elems
              else gets $ robotsInArea loc hearingDistance
        mapM_ addToRobotLog robotsAround
        return $ mkReturn ()
      _ -> badConst
    Listen -> do
      gs <- get @GameState
      loc <- use robotLocation
      rid <- use robotID
      isPrivileged <- isPrivilegedBot
      mq <- use $ messageInfo . messageQueue
      let isClose e = isPrivileged || messageIsFromNearby loc e
          notMine e = case e ^. leSource of
            SystemLog {} -> False
            RobotLog _ lrid _ -> rid /= lrid
          limitLast = \case
            _s Seq.:|> l -> Just $ l ^. leText
            _ -> Nothing
          mm = limitLast . Seq.filter (liftA2 (&&) notMine isClose) $ Seq.takeWhileR (messageIsRecent gs) mq
      return $
        maybe
          (In (TConst Listen) mempty s (FExec : k)) -- continue listening
          (\m -> Out (VText m) s k) -- return found message
          mm
    Log -> case vs of
      [VText msg] -> do
        void $ traceLog Logged Info msg
        return $ mkReturn ()
      _ -> badConst
    View -> case vs of
      [VRobot rid] -> do
        -- Only the base can actually change the view in the UI.  Other robots can
        -- execute this command but it does nothing (at least for now).
        rn <- use robotID
        when (rn == 0) $
          robotWithID rid >>= \case
            -- If the robot does not exist...
            Nothing -> do
              cr <- use creativeMode
              ws <- use $ landscape . worldScrollable
              case cr || ws of
                -- If we are in creative mode or allowed to scroll, then we are allowed
                -- to learn that the robot doesn't exist.
                True -> throwError $ cmdExn c ["There is no actor with ID", from (show rid), "to view."]
                -- Otherwise, "unfocus" from any robot, which
                -- means the world view will turn to static.  The
                -- point is that there's no way to tell the difference
                -- between this situation and the situation where the
                -- robot exists but is too far away.
                False -> robotInfo %= unfocus

            -- If it does exist, set it as the view center.
            Just _ -> robotInfo . viewCenterRule .= VCRobot rid

        return $ mkReturn ()
      _ -> badConst
    Appear -> case vs of
      [VText app] -> do
        flagRedraw
        case into @String app of
          [dc] -> do
            robotDisplay . defaultChar .= dc
            robotDisplay . orientationMap .= M.empty
            return $ mkReturn ()
          [dc, nc, ec, sc, wc] -> do
            robotDisplay . defaultChar .= dc
            robotDisplay . orientationMap . ix DNorth .= nc
            robotDisplay . orientationMap . ix DEast .= ec
            robotDisplay . orientationMap . ix DSouth .= sc
            robotDisplay . orientationMap . ix DWest .= wc
            return $ mkReturn ()
          _other -> raise Appear [quote app, "is not a valid appearance string. 'appear' must be given a string with exactly 1 or 5 characters."]
      _ -> badConst
    Create -> case vs of
      [VText name] -> do
        em <- use $ landscape . entityMap
        e <-
          lookupEntityName name em
            `isJustOrFail` ["I've never heard of", indefiniteQ name <> "."]

        robotInventory %= insert e
        updateDiscoveredEntities e

        return $ mkReturn ()
      _ -> badConst
    Halt -> case vs of
      [VRobot targetID] -> do
        myID <- use robotID
        case myID == targetID of
          -- To halt ourselves, just return a cancelled CESK machine.
          -- It will be reinstalled as our current machine; then,
          -- based on the fact that our CESK machine is done we will
          -- be put to sleep and the REPL will be reset if we are the
          -- base robot.
          True -> return $ cancel $ mkReturn ()
          False -> do
            -- Make sure the other robot exists and is close enough.
            target <- getRobotWithinTouch targetID
            -- Make sure either we are privileged, OR the target robot
            -- is NOT.  In other words unprivileged bots should not be
            -- able to halt privileged ones.
            omni <- isPrivilegedBot
            case omni || not (target ^. systemRobot) of
              True -> zoomRobots $ do
                -- Cancel its CESK machine, and put it to sleep.
                robotMap . at targetID . _Just . machine %= cancel
                sleepForever targetID
                return $ mkReturn ()
              False -> throwError $ cmdExn c ["You are not authorized to halt that robot."]
      _ -> badConst
    Ishere -> case vs of
      [VText name] -> do
        loc <- use robotLocation
        me <- entityAt loc
        let here = maybe False (isEntityNamed name) me
        return $ mkReturn here
      _ -> badConst
    Isempty -> do
      loc <- use robotLocation
      me <- entityAt loc
      return $ mkReturn $ isNothing me
    Self -> do
      rid <- use robotID
      return $ Out (VRobot rid) s k
    Parent -> do
      mp <- use robotParentID
      rid <- use robotID
      return $ Out (VRobot (fromMaybe rid mp)) s k
    Base -> return $ Out (VRobot 0) s k
    Meet -> do
      loc <- use robotLocation
      rid <- use robotID
      g <- get @GameState
      let neighbor =
            find ((/= rid) . (^. robotID)) -- pick one other than ourself
              . sortOn ((manhattan `on` view planar) loc . (^. robotLocation)) -- prefer closer
              $ robotsInArea loc 1
              $ g ^. robotInfo -- all robots within Manhattan distance 1
      return $ mkReturn neighbor
    MeetAll -> case vs of
      [f, b] -> do
        loc <- use robotLocation
        rid <- use robotID
        g <- get @GameState
        let neighborIDs = filter (/= rid) . map (^. robotID) $ robotsInArea loc 1 $ g ^. robotInfo
        return $ Out b s (FMeetAll f neighborIDs : k)
      _ -> badConst
    Whoami -> case vs of
      [] -> do
        name <- use robotName
        return $ mkReturn name
      _ -> badConst
    Setname -> case vs of
      [VText name] -> do
        robotName .= name
        return $ mkReturn ()
      _ -> badConst
    Force -> case vs of
      [VDelay t e] -> return $ In t e s k
      [VRef loc] ->
        -- To force a VRef, we look up the location in the store.
        case lookupStore loc s of
          -- If there's no cell at that location, it's a bug!  It
          -- shouldn't be possible to get a VRef to a non-existent
          -- location, since the only way VRefs get created is at the
          -- time we allocate a new cell.
          Nothing ->
            return $
              Up (Fatal $ T.append "Reference to unknown memory cell " (from (show loc))) s k
          -- If the location contains an unevaluated expression, it's
          -- time to evaluate it.  Set the cell to a 'Blackhole', push
          -- an 'FUpdate' frame so we remember to update the location
          -- to its value once we finish evaluating it, and focus on
          -- the expression.
          Just (E t e') -> return $ In t e' (setStore loc (Blackhole t e') s) (FUpdate loc : k)
          -- If the location contains a Blackhole, that means we are
          -- already currently in the middle of evaluating it, i.e. it
          -- depends on itself, so throw an 'InfiniteLoop' error.
          Just Blackhole {} -> return $ Up InfiniteLoop s k
          -- If the location already contains a value, just return it.
          Just (V v) -> return $ Out v s k
      -- If a force is applied to any other kind of value, just ignore it.
      -- This is needed because of the way we wrap all free variables in @force@
      -- in case they come from a @def@ which are always wrapped in @delay@.
      -- But binders (i.e. @x <- ...@) are also exported to the global context.
      [v] -> return $ Out v s k
      _ -> badConst
    If -> case vs of
      -- Use the boolean to pick the correct branch, and apply @force@ to it.
      [VBool b, thn, els] -> return $ Out (bool els thn b) s (FApp (VCApp Force []) : k)
      _ -> badConst
    Inl -> case vs of
      [v] -> return $ Out (VInj False v) s k
      _ -> badConst
    Inr -> case vs of
      [v] -> return $ Out (VInj True v) s k
      _ -> badConst
    Case -> case vs of
      [VInj side v, kl, kr] -> return $ Out v s (FApp (bool kl kr side) : k)
      _ -> badConst
    Fst -> case vs of
      [VPair v _] -> return $ Out v s k
      _ -> badConst
    Snd -> case vs of
      [VPair _ v] -> return $ Out v s k
      _ -> badConst
    Try -> case vs of
      [c1, c2] -> return $ Out c1 s (FApp (VCApp Force []) : FExec : FTry c2 : k)
      _ -> badConst
    Undefined -> return $ Up (User "undefined") s k
    Fail -> case vs of
      [VText msg] -> return $ Up (User msg) s k
      _ -> badConst
    Key -> case vs of
      [VText ktxt] -> case runParser parseKeyComboFull ktxt of
        Right kc -> return $ Out (VKey kc) s k
        Left _ -> return $ Up (CmdFailed Key (T.unwords ["Unknown key", quote ktxt]) Nothing) s k
      _ -> badConst
    InstallKeyHandler -> case vs of
      [VText hint, handler] -> do
        gameControls . inputHandler .= Just (hint, handler)
        return $ mkReturn ()
      _ -> badConst
    Reprogram -> case vs of
      [VRobot childRobotID, VDelay cmd e] -> do
        r <- get
        isPrivileged <- isPrivilegedBot

        -- check if robot exists
        childRobot <-
          robotWithID childRobotID
            >>= (`isJustOrFail` ["There is no actor with ID", from (show childRobotID) <> "."])

        -- check that current robot is not trying to reprogram self
        myID <- use robotID
        (childRobotID /= myID)
          `holdsOrFail` ["You cannot make a robot reprogram itself."]

        -- check if robot has completed executing it's current command
        _ <-
          finalValue (childRobot ^. machine)
            `isJustOrFail` ["You cannot reprogram a robot that is actively running a program."]

        -- check if childRobot is at the correct distance
        -- a robot can program adjacent robots
        -- privileged bots ignore distance checks
        loc <- use robotLocation

        isNearbyOrExempt isPrivileged loc (childRobot ^. robotLocation)
          `holdsOrFail` ["You can only reprogram an adjacent robot."]

        -- Figure out if we can supply what the target robot requires,
        -- and if so, what is needed.
        (toEquip, toGive) <-
          checkRequirements
            (r ^. robotInventory)
            (childRobot ^. robotInventory)
            (childRobot ^. equippedDevices)
            cmd
            "The target robot"
            FixByObtain

        -- update other robot's CESK machine, environment and context
        -- the childRobot inherits the parent robot's environment
        -- and context which collectively mean all the variables
        -- declared in the parent robot
        zoomRobots $ do
          robotMap . at childRobotID . _Just . machine .= In cmd e s [FExec]
          robotMap . at childRobotID . _Just . robotContext .= r ^. robotContext

        -- Provision the target robot with any required devices and
        -- inventory that are lacking.
        provisionChild childRobotID (fromList . S.toList $ toEquip) toGive

        -- Finally, re-activate the reprogrammed target robot.
        zoomRobots $ activateRobot childRobotID

        return $ mkReturn ()
      _ -> badConst
    Build -> case vs of
      -- NOTE, pattern-matching on a VDelay here means we are
      -- /relying/ on the fact that 'Build' can only be given a
      -- /non-memoized/ delayed value.  If it were given a memoized
      -- delayed value we would see a VRef instead of a VDelay.  If
      -- and Try are generalized to handle any type of delayed value,
      -- but Build and Reprogram still assume they are given a VDelay
      -- and not a VRef.  In the future, if we enable memoized delays
      -- by default, or allow the user to explicitly request
      -- memoization via double braces or something similar, this will
      -- have to be generalized.  The difficulty is that we do a
      -- capability check on the delayed program at runtime, just
      -- before creating the newly built robot (see the call to
      -- 'requirements' below); but if we have a VRef instead of a
      -- VDelay, we may only be able to get a Value out of it instead
      -- of a Term as we currently do, and capability checking a Value
      -- is annoying and/or problematic.  One solution might be to
      -- annotate delayed expressions with their required capabilities
      -- at typechecking time, and carry those along so they flow to
      -- this point. Another solution would be to just bite the bullet
      -- and figure out how to do capability checking on Values (which
      -- would return the capabilities needed to *execute* them),
      -- hopefully without duplicating too much code.
      [VDelay cmd e] -> do
        r <- get @Robot
        pid <- use robotID

        (toEquip, toGive) <-
          checkRequirements (r ^. robotInventory) E.empty E.empty cmd "You" FixByObtain

        -- Pick a random display name.
        displayName <- randomName
        createdAt <- getNow
        isSystemRobot <- use systemRobot

        -- Construct the new robot and add it to the world.
        parentCtx <- use robotContext
        newRobot <-
          zoomRobots . addTRobot . (trobotContext .~ parentCtx) $
            mkRobot
              ()
              (Just pid)
              displayName
              (Markdown.fromText $ "A robot built by the robot named " <> (r ^. robotName) <> ".")
              (Just (r ^. robotLocation))
              ( ((r ^. robotOrientation) >>= \dir -> guard (dir /= zero) >> return dir)
                  ? north
              )
              ((r ^. robotDisplay) & invisible .~ False)
              (In cmd e s [FExec])
              []
              []
              isSystemRobot
              False
              mempty
              createdAt

        -- Provision the new robot with the necessary devices and inventory.
        provisionChild (newRobot ^. robotID) (fromList . S.toList $ toEquip) toGive

        -- Flag the world for a redraw and return the ID of the newly constructed robot.
        flagRedraw
        return $ mkReturn newRobot
      _ -> badConst
    Salvage -> case vs of
      [] -> do
        loc <- use robotLocation
        let okToSalvage r = (r ^. robotID /= 0) && (not . isActive $ r)
        mtarget <- gets (find okToSalvage . robotsAtLocation loc)
        case mtarget of
          Nothing -> return $ mkReturn () -- Nothing to salvage
          Just target -> do
            -- Copy the salvaged robot's equipped devices into its inventory, in preparation
            -- for transferring it.
            let salvageInventory = E.union (target ^. robotInventory) (target ^. equippedDevices)
            robotInfo . robotMap . at (target ^. robotID) . traverse . robotInventory .= salvageInventory

            let salvageItems = concatMap (\(n, e) -> replicate n (e ^. entityName)) (E.elems salvageInventory)
                numItems = length salvageItems

            -- Copy over the salvaged robot's log, if we have one
            inst <- use equippedDevices
            em <- use $ landscape . entityMap
            isPrivileged <- isPrivilegedBot
            logger <-
              lookupEntityName "logger" em
                `isJustOr` Fatal "While executing 'salvage': there's no such thing as a logger!?"
            when (isPrivileged || inst `E.contains` logger) $ robotLog <>= target ^. robotLog

            -- Immediately copy over any items the robot knows about
            -- but has 0 of
            let knownItems = map snd . filter ((== 0) . fst) . elems $ salvageInventory
            robotInventory %= \i -> foldr (insertCount 0) i knownItems

            -- Now reprogram the robot being salvaged to 'give' each
            -- item in its inventory to us, one at a time, then
            -- self-destruct at the end.  Make it a system robot so we
            -- don't have to worry about capabilities.
            robotInfo . robotMap . at (target ^. robotID) . traverse . systemRobot .= True

            ourID <- use @Robot robotID

            -- The program for the salvaged robot to run
            let giveInventory =
                  foldr (TBind Nothing . giveItem) (TConst Selfdestruct) salvageItems
                giveItem item = TApp (TApp (TConst Give) (TRobot ourID)) (TText item)

            -- Reprogram and activate the salvaged robot
            zoomRobots $ do
              robotMap
                . at (target ^. robotID)
                . traverse
                . machine
                .= In giveInventory empty emptyStore [FExec]

              activateRobot $ target ^. robotID

            -- Now wait the right amount of time for it to finish.
            time <- use $ temporal . ticks
            return $ Waiting (addTicks (numItems + 1) time) (mkReturn ())
      _ -> badConst
    -- run can take both types of text inputs
    -- with and without file extension as in
    -- "./path/to/file.sw" and "./path/to/file"
    Run -> case vs of
      [VText fileName] -> do
        let filePath = into @String fileName
        sData <- throwToMaybe @SystemFailure $ getDataFileNameSafe Script filePath
        sDataSW <- throwToMaybe @SystemFailure $ getDataFileNameSafe Script (filePath <> ".sw")
        mf <- sendIO $ mapM readFileMay $ [filePath, filePath <> ".sw"] <> catMaybes [sData, sDataSW]

        f <- msum mf `isJustOrFail` ["File not found:", fileName]

        mt <-
          processTerm (into @Text f) `isRightOr` \err ->
            cmdExn Run ["Error in", fileName, "\n", err]

        case mt of
          Nothing -> return $ mkReturn ()
          Just t@(ProcessedTerm _ _ reqCtx) -> do
            -- Add the reqCtx from the ProcessedTerm to the current robot's defReqs.
            -- See #827 for an explanation of (1) why this is needed, (2) why
            -- it's slightly technically incorrect, and (3) why it is still way
            -- better than what we had before.
            robotContext . defReqs <>= reqCtx
            return $ initMachine' t empty s k
      _ -> badConst
    Not -> case vs of
      [VBool b] -> return $ Out (VBool (not b)) s k
      _ -> badConst
    Neg -> case vs of
      [VInt n] -> return $ Out (VInt (-n)) s k
      _ -> badConst
    Eq -> returnEvalCmp
    Neq -> returnEvalCmp
    Lt -> returnEvalCmp
    Gt -> returnEvalCmp
    Leq -> returnEvalCmp
    Geq -> returnEvalCmp
    And -> case vs of
      [VBool a, VBool b] -> return $ Out (VBool (a && b)) s k
      _ -> badConst
    Or -> case vs of
      [VBool a, VBool b] -> return $ Out (VBool (a || b)) s k
      _ -> badConst
    Add -> returnEvalArith
    Sub -> returnEvalArith
    Mul -> returnEvalArith
    Div -> returnEvalArith
    Exp -> returnEvalArith
    Format -> case vs of
      [v] -> return $ mkReturn $ prettyValue v
      _ -> badConst
    Chars -> case vs of
      [VText t] -> return $ mkReturn $ T.length t
      _ -> badConst
    Split -> case vs of
      [VInt i, VText t] ->
        let p = T.splitAt (fromInteger i) t
            t2 = over both VText p
         in return $ Out (uncurry VPair t2) s k
      _ -> badConst
    Concat -> case vs of
      [VText v1, VText v2] -> return $ mkReturn $ v1 <> v2
      _ -> badConst
    CharAt -> case vs of
      [VInt i, VText t]
        | i < 0 || i >= fromIntegral (T.length t) ->
            raise CharAt ["Index", prettyValue (VInt i), "out of bounds for length", from @String $ show (T.length t)]
        | otherwise -> return . mkReturn . ord . T.index t . fromIntegral $ i
      _ -> badConst
    ToChar -> case vs of
      [VInt i]
        | i < 0 || i > fromIntegral (ord (maxBound :: Char)) ->
            raise ToChar ["Value", prettyValue (VInt i), "is an invalid character code"]
        | otherwise ->
            return . mkReturn . T.singleton . chr . fromIntegral $ i
      _ -> badConst
    AppF ->
      let msg = "The operator '$' should only be a syntactic sugar and removed in elaboration:\n"
       in throwError . Fatal $ msg <> badConstMsg
 where
  doDrill d = do
    ins <- use equippedDevices

    let equippedDrills = extantElemsWithCapability CDrill ins
        -- Heuristic: choose the drill with the more elaborate name.
        -- E.g. "metal drill" vs. "drill"
        preferredDrill = listToMaybe $ sortOn (Down . T.length . (^. entityName)) equippedDrills

    tool <- preferredDrill `isJustOr` Fatal "Drill is required but not equipped?!"
    applyDevice ins "drill" d tool

  applyDevice ins verbPhrase d tool = do
    (nextLoc, nextE) <- getDeviceTarget verbPhrase d
    inRs <- use $ recipesInfo . recipesIn

    let recipes = filter isApplicableRecipe (recipesFor inRs nextE)
        isApplicableRecipe = any ((== tool) . snd) . view recipeCatalysts

    not (null recipes)
      `holdsOrFail` [ "There is no way to"
                    , verbPhrase
                    , indefinite (nextE ^. entityName) <> "."
                    ]

    inv <- use robotInventory

    -- add the targeted entity so it can be consumed by the recipe
    let makeRecipe r = (,r) <$> make' (insert nextE inv, ins) r
    chosenRecipe <-
      weightedChoice (\((_, _), r) -> r ^. recipeWeight) $
        rights $
          map makeRecipe recipes
    ((invTaken, outs), recipe) <-
      chosenRecipe
        `isJustOrFail` ["You don't have the ingredients to", verbPhrase, indefinite (nextE ^. entityName) <> "."]

    let (out, down) = L.partition ((`hasProperty` Portable) . snd) outs
        learn = map (LearnEntity . snd) down
        gain = map (uncurry AddEntity) out

    newEnt <- case down of
      [] -> pure Nothing
      [(1, de)] -> pure $ Just de
      _ -> throwError $ Fatal "Bad recipe:\n more than one unmovable entity produced."
    let changeWorld =
          ReplaceEntity
            { updatedLoc = nextLoc
            , originalEntity = nextE
            , newEntity = newEnt
            }

    -- take recipe inputs from inventory and add outputs after recipeTime
    robotInventory .= invTaken

    let cmdOutput = asValue $ snd <$> listToMaybe out
    finishCookingRecipe recipe cmdOutput [changeWorld] (learn <> gain)

  getDeviceTarget verb d = do
    rname <- use robotName

    (nextLoc, nextME) <- lookInDirection d
    nextE <-
      nextME
        `isJustOrFail` ["There is nothing to", verb, directionText, "robot", rname <> "."]
    return (nextLoc, nextE)
   where
    directionText = case d of
      DRelative DDown -> "under"
      DRelative (DPlanar DForward) -> "ahead of"
      DRelative (DPlanar DBack) -> "behind"
      _ -> directionSyntax d <> " of"

  goAtomic :: HasRobotStepState sig m => m CESK
  goAtomic = case vs of
    -- To execute an atomic block, set the runningAtomic flag,
    -- push an FFinishAtomic frame so that we unset the flag when done, and
    -- proceed to execute the argument.
    [cmd] -> do
      runningAtomic .= True
      return $ Out cmd s (FExec : FFinishAtomic : k)
    _ -> badConst

  -- Case-insensitive matching on entity names
  isEntityNamed :: T.Text -> Entity -> Bool
  isEntityNamed n e = ((==) `on` T.toLower) (e ^. entityName) n

  badConst :: HasRobotStepState sig m => m a
  badConst = throwError $ Fatal badConstMsg

  badConstMsg :: Text
  badConstMsg =
    T.unlines
      [ "Bad application of execConst:"
      , T.pack (show c)
      , T.pack (show (reverse vs))
      , prettyText (Out (VCApp c (reverse vs)) s k)
      ]

  doResonate ::
    (HasRobotStepState sig m, Has (Lift IO) sig m) =>
    (Maybe Entity -> Bool) ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    m CESK
  doResonate p x1 y1 x2 y2 = do
    loc <- use robotLocation
    let offsets = rectCells x1 y1 x2 y2
    hits <- mapM (fmap (fromEnum . p) . entityAt . offsetBy loc) offsets
    return $ Out (VInt $ fromIntegral $ sum hits) s k

  rectCells :: Integer -> Integer -> Integer -> Integer -> [V2 Int32]
  rectCells x1 y1 x2 y2 =
    rectCellsInt32
      (fromIntegral x1)
      (fromIntegral y1)
      (fromIntegral x2)
      (fromIntegral y2)

  rectCellsInt32 :: Int32 -> Int32 -> Int32 -> Int32 -> [V2 Int32]
  rectCellsInt32 x1 y1 x2 y2 = [V2 x y | x <- [xMin .. xMax], y <- [yMin .. yMax]]
   where
    (xMin, xMax) = sortPair (x1, x2)
    (yMin, yMax) = sortPair (y1, y2)

  findNearest ::
    HasRobotStepState sig m =>
    Text ->
    m (Maybe (Int32, V2 Int32))
  findNearest name = do
    loc <- use robotLocation
    let f = fmap (maybe False $ isEntityNamed name) . entityAt . offsetBy loc . snd
    findM f sortedOffsets
   where
    sortedOffsets :: [(Int32, V2 Int32)]
    sortedOffsets = (0, zero) : concatMap genDiamondSides [1 .. maxSniffRange]

    -- Grow a list of locations in a diamond shape outward, such that the nearest cells
    -- are searched first by construction, rather than having to sort.
    genDiamondSides :: Int32 -> [(Int32, V2 Int32)]
    genDiamondSides diameter = concat [f diameter x | x <- [0 .. diameter]]
     where
      -- Adds a single cell to each of the four sides of the diamond
      f d x = map (d,) . take 4 . iterate perp $ V2 x (d - x)

  finishCookingRecipe ::
    HasRobotStepState sig m =>
    Recipe e ->
    Value ->
    [WorldUpdate Entity] ->
    [RobotUpdate] ->
    m CESK
  finishCookingRecipe r v wf rf =
    if remTime <= 0
      then do
        updateWorldAndRobots c wf rf
        return $ Out v s k
      else do
        time <- use $ temporal . ticks
        return . (if remTime <= 1 then id else Waiting (addTicks (fromIntegral remTime) time)) $
          Out v s (FImmediate c wf rf : k)
   where
    remTime = r ^. recipeTime

  ensureEquipped :: HasRobotStepState sig m => Text -> m Entity
  ensureEquipped itemName = do
    inst <- use equippedDevices
    listToMaybe (lookupByName itemName inst)
      `isJustOrFail` ["You don't have a", indefinite itemName, "equipped."]

  ensureItem :: HasRobotStepState sig m => Text -> Text -> m Entity
  ensureItem itemName action = do
    -- First, make sure we know about the entity.
    inv <- use robotInventory
    inst <- use equippedDevices
    item <-
      asum (map (listToMaybe . lookupByName itemName) [inv, inst])
        `isJustOrFail` ["What is", indefinite itemName <> "?"]

    -- Next, check whether we have one.  If we don't, add a hint about
    -- 'create' in creative mode.
    creative <- use creativeMode
    let create l = l <> ["You can make one first with 'create \"" <> itemName <> "\"'." | creative]

    (E.lookup item inv > 0)
      `holdsOrFail` create ["You don't have", indefinite itemName, "to", action <> "."]

    return item

  -- Check the required devices and inventory for running the given
  -- command on a target robot.  This function is used in common by
  -- both 'Build' and 'Reprogram'.
  --
  -- It is given as inputs the parent robot inventory, the inventory
  -- and equipped devices of the child (these will be empty in the
  -- case of 'Build'), and the command to be run (along with a few
  -- inputs to configure any error messages to be generated).
  --
  -- Throws an exception if it's not possible to set up the child
  -- robot with the things it needs to execute the given program.
  -- Otherwise, returns a pair consisting of the set of devices to be
  -- equipped, and the inventory that should be transferred from
  -- parent to child.
  checkRequirements ::
    HasRobotStepState sig m =>
    Inventory ->
    Inventory ->
    Inventory ->
    Term ->
    Text ->
    IncapableFix ->
    m (Set Entity, Inventory)
  checkRequirements parentInventory childInventory childDevices cmd subject fixI = do
    currentContext <- use $ robotContext . defReqs
    em <- use $ landscape . entityMap
    creative <- use creativeMode
    let -- Note that _capCtx must be empty: at least at the
        -- moment, definitions are only allowed at the top level,
        -- so there can't be any inside the argument to build.
        -- (Though perhaps there is an argument that this ought to be
        -- relaxed specifically in the cases of 'Build' and 'Reprogram'.)
        -- See #349
        (R.Requirements (S.toList -> caps) (S.toList -> devNames) reqInvNames, _capCtx) = R.requirements currentContext cmd

    -- Check that all required device names exist (fail with
    -- an exception if not) and convert them to 'Entity' values.
    (devs :: [Entity]) <- forM devNames $ \devName ->
      E.lookupEntityName devName em `isJustOrFail` ["Unknown device required: " <> devName]

    -- Check that all required inventory entity names exist (fail with
    -- an exception if not) and convert them to 'Entity' values, with
    -- an associated count for each.
    (reqInv :: Inventory) <- fmap E.fromElems . forM (M.assocs reqInvNames) $ \(eName, n) ->
      (n,)
        <$> ( E.lookupEntityName eName em
                `isJustOrFail` ["Unknown entity required: " <> eName]
            )

    let -- List of possible devices per requirement.  For the
        -- requirements that stem from a required capability, we
        -- remember the capability alongside the possible devices, to
        -- help with later error message generation.
        possibleDevices :: [(Maybe Capability, [Entity])]
        possibleDevices =
          map (Just &&& (`deviceForCap` em)) caps -- Possible devices for capabilities
            ++ map ((Nothing,) . (: [])) devs -- Outright required devices

        -- A device is OK if it is available in the inventory of the
        -- parent robot, or already equipped in the child robot.
        deviceOK :: Entity -> Bool
        deviceOK d = parentInventory `E.contains` d || childDevices `E.contains` d

        -- Partition each list of possible devices into a set of
        -- available devices and a set of unavailable devices.
        -- There's a problem if some capability is required but no
        -- devices that provide it are available.  In that case we can
        -- print an error message, using the second set as a list of
        -- suggestions.
        partitionedDevices :: [(Set Entity, Set Entity)]
        partitionedDevices =
          map (Lens.over both S.fromList . L.partition deviceOK . snd) possibleDevices

        -- Devices equipped on the child, as a Set instead of an
        -- Inventory for convenience.
        alreadyEquipped :: Set Entity
        alreadyEquipped = S.fromList . map snd . E.elems $ childDevices

        -- Figure out what is still missing of the required inventory:
        -- the required inventory, less any inventory the child robot
        -- already has.
        missingChildInv = reqInv `E.difference` childInventory

    if creative
      then
        return
          ( -- In creative mode, just equip ALL the devices
            -- providing each required capability (because, why
            -- not?). But don't re-equip any that are already
            -- equipped.
            S.unions (map (S.fromList . snd) possibleDevices) `S.difference` alreadyEquipped
          , -- Conjure the necessary missing inventory out of thin
            -- air.
            missingChildInv
          )
      else do
        -- First, check that devices actually exist AT ALL to provide every
        -- required capability.  If not, we will generate an error message saying
        -- something like "missing capability X but no device yet provides it".
        let capsWithNoDevice = mapMaybe fst . filter (null . snd) $ possibleDevices
        null capsWithNoDevice
          `holdsOr` Incapable fixI (R.Requirements (S.fromList capsWithNoDevice) S.empty M.empty) cmd

        -- Now, ensure there is at least one device available to be
        -- equipped for each requirement.
        let missingDevices = map snd . filter (null . fst) $ partitionedDevices
        null missingDevices
          `holdsOrFail` ( singularSubjectVerb subject "do"
                            : "not have required devices, please"
                            : formatIncapableFix fixI <> ":"
                            : (("\n  - " <>) . formatDevices <$> missingDevices)
                        )

        let minimalEquipSet = smallHittingSet (filter (S.null . S.intersection alreadyEquipped) (map fst partitionedDevices))

            -- Check that we have enough in our inventory to cover the
            -- required devices PLUS what's missing from the child
            -- inventory.

            -- What do we need?
            neededParentInv =
              missingChildInv
                `E.union` (fromList . S.toList $ minimalEquipSet)

            -- What are we missing?
            missingParentInv = neededParentInv `E.difference` parentInventory
            missingMap =
              M.fromList
                . filter ((> 0) . snd)
                . map (swap . second (^. entityName))
                . E.elems
                $ missingParentInv

        -- If we're missing anything, throw an error
        E.isEmpty missingParentInv
          `holdsOr` Incapable fixI (R.Requirements S.empty S.empty missingMap) cmd

        return (minimalEquipSet, missingChildInv)

  -- Destroy the current robot, as long as it is not the base robot.
  --
  -- Depending on whether we destroy (True) or do not destroy
  -- (False) the current robot, possibly grant an achievement.
  --
  -- Note we cannot simply return a Boolean and grant achievements
  -- at call sites, because in the case that we do not destroy the
  -- base we actually throw an exception, so we do not return to the
  -- original call site.
  destroyIfNotBase ::
    (HasRobotStepState sig m, Has (Lift IO) sig m) =>
    (Bool -> Maybe GameplayAchievement) ->
    m ()
  destroyIfNotBase mAch = do
    rid <- use robotID
    holdsOrFailWithAchievement
      (rid /= 0)
      ["You consider destroying your base, but decide not to do it after all."]
      (mAch False)

    selfDestruct .= True
    maybe (return ()) grantAchievement (mAch True)

  moveInDirection :: (HasRobotStepState sig m, Has (Lift IO) sig m) => Heading -> m CESK
  moveInDirection orientation = do
    -- Figure out where we're going
    loc <- use robotLocation
    let nextLoc = loc `offsetBy` orientation
    checkMoveAhead nextLoc $ \case
      PathBlocked -> ThrowExn
      PathLiquid -> Destroy
    updateRobotLocation loc nextLoc
    return $ mkReturn ()

  applyMoveFailureEffect ::
    (HasRobotStepState sig m, Has (Lift IO) sig m) =>
    Maybe MoveFailureDetails ->
    MoveFailureHandler ->
    m ()
  applyMoveFailureEffect maybeFailure failureHandler =
    case maybeFailure of
      Nothing -> return ()
      Just (MoveFailureDetails e failureMode) -> case failureHandler failureMode of
        IgnoreFail -> return ()
        Destroy -> destroyIfNotBase $ \b -> case (b, failureMode) of
          (True, PathLiquid) -> Just RobotIntoWater -- achievement for drowning
          _ -> Nothing
        ThrowExn -> throwError . cmdExn c $
          case failureMode of
            PathBlocked -> ["There is a", e ^. entityName, "in the way!"]
            PathLiquid -> ["There is a dangerous liquid", e ^. entityName, "in the way!"]

  -- Determine the move failure mode and apply the corresponding effect.
  checkMoveAhead ::
    (HasRobotStepState sig m, Has (Lift IO) sig m) =>
    Cosmic Location ->
    MoveFailureHandler ->
    m ()
  checkMoveAhead nextLoc failureHandler = do
    maybeFailure <- checkMoveFailure nextLoc
    applyMoveFailureEffect maybeFailure failureHandler

  getRobotWithinTouch :: HasRobotStepState sig m => RID -> m Robot
  getRobotWithinTouch rid = do
    cid <- use robotID
    if cid == rid
      then get @Robot
      else do
        mother <- robotWithID rid
        other <- mother `isJustOrFail` ["There is no robot with ID", from (show rid) <> "."]

        let otherLoc = other ^. robotLocation
        privileged <- isPrivilegedBot
        myLoc <- use robotLocation

        -- Make sure it is either in the same location or we do not care
        isNearbyOrExempt privileged myLoc otherLoc
          `holdsOrFail` ["The robot with ID", from (show rid), "is not close enough."]
        return other

  holdsOrFail :: (Has (Throw Exn) sig m) => Bool -> [Text] -> m ()
  holdsOrFail = holdsOrFail' c

  holdsOrFailWithAchievement :: (Has (Throw Exn) sig m) => Bool -> [Text] -> Maybe GameplayAchievement -> m ()
  holdsOrFailWithAchievement a ts mAch = case mAch of
    Nothing -> holdsOrFail a ts
    Just ach -> a `holdsOr` cmdExnWithAchievement c ts ach

  isJustOrFail :: (Has (Throw Exn) sig m) => Maybe a -> [Text] -> m a
  isJustOrFail = isJustOrFail' c

  returnEvalCmp = case vs of
    [v1, v2] -> (\b -> Out (VBool b) s k) <$> evalCmp c v1 v2
    _ -> badConst
  returnEvalArith = case vs of
    [VInt n1, VInt n2] -> (\r -> Out (VInt r) s k) <$> evalArith c n1 n2
    _ -> badConst

  -- Make sure the robot has the thing in its inventory
  hasInInventoryOrFail :: HasRobotStepState sig m => Text -> m Entity
  hasInInventoryOrFail eName = do
    inv <- use robotInventory
    e <-
      listToMaybe (lookupByName eName inv)
        `isJustOrFail` ["What is", indefinite eName <> "?"]
    let cmd = T.toLower . T.pack . show $ c
    (E.lookup e inv > 0)
      `holdsOrFail` ["You don't have", indefinite eName, "to", cmd <> "."]
    return e

  mkReturn :: Valuable a => a -> CESK
  mkReturn x = Out (asValue x) s k

  -- The code for grab and harvest is almost identical, hence factored
  -- out here.
  doGrab :: (HasRobotStepState sig m, Has Effect.Time sig m) => GrabbingCmd -> m Entity
  doGrab cmd = do
    let verb = verbGrabbingCmd cmd
        verbed = verbedGrabbingCmd cmd

    -- Ensure there is an entity here.
    loc <- use robotLocation
    e <-
      entityAt loc
        >>= (`isJustOrFail` ["There is nothing here to", verb <> "."])

    -- Ensure it can be picked up.
    omni <- isPrivilegedBot
    (omni || e `hasProperty` Portable)
      `holdsOrFail` ["The", e ^. entityName, "here can't be", verbed <> "."]

    -- Remove the entity from the world.
    updateEntityAt loc (const Nothing)
    flagRedraw

    -- Immediately regenerate entities with 'infinite' property.
    when (e `hasProperty` Infinite) $
      updateEntityAt loc (const (Just e))

    -- Possibly regrow the entity, if it is growable and the 'harvest'
    -- command was used.
    when ((e `hasProperty` Growable) && cmd == Harvest') $ do
      let GrowthTime (minT, maxT) = (e ^. entityGrowth) ? defaultGrowthTime

      createdAt <- getNow

      -- Grow a new entity from a seed.
      addSeedBot e (minT, maxT) loc createdAt

    -- Add the picked up item to the robot's inventory.  If the
    -- entity yields something different, add that instead.
    let yieldName = e ^. entityYields
    e' <- case yieldName of
      Nothing -> return e
      Just n -> fromMaybe e <$> uses (landscape . entityMap) (lookupEntityName n)

    robotInventory %= insert e'
    updateDiscoveredEntities e'

    -- Return the item obtained.
    return e'
