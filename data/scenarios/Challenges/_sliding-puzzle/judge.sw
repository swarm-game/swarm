def id = \t. t end
def elif = \t. \then. \else. {if t then else} end
def else = id end

def mod : int -> int -> int = \i.\m.
  i - m * (i / m)
end

def isEven = \n.
    mod n 2 == 0;
    end

def sumTuples = \t1. \t2.
    (fst t1 + fst t2, snd t1 + snd t2);
    end;

/** Teleports to a new location to execute a function
  then returns to the original location before
  returning the functions output value.
*/
def atLocation = \newLoc. \f.
    prevLoc <- whereami;
    teleport self newLoc;
    retval <- f;
    teleport self prevLoc;
    return retval;
    end;

/**
Returns true if should terminate the parent
function's recursion due to goal being met.
*/
def intersperseUntil = \n. \f2. \f1.
    if (n > 0) {
        shouldTerminate <- f1;
        if shouldTerminate {
            return true;
        } {
            if (n > 1) {
                f2;
            } {};
            intersperseUntil (n - 1) f2 f1;
        };
    } {
        return false;
    };
    end;

def getDirection = \n.
    if (n == 0) {
        forward;
    } $ elif (n == 1) {
        right;
    } $ elif (n == 2) {
        back;
    } $ elif (n == 3) {
        left;
    } $ else {
        down;
    };
    end;

def watchDir = \n.
    watch $ getDirection n;
    if (n > 0) {
        watchDir $ n - 1;
    } {};
    end;

def getOrdinal : text -> cmd int = \item.
    count $ item ++ "-ordinal";
    end;

def getValueHere =
    maybeItem <- scan down;
    ordNum <- case maybeItem (\_. return 0) getOrdinal;
    end;

/**
  Swaps the element at the current position
  with the element "x" cells away.
  Useful for an in-place sort.

  Precondition: Facing east.
*/
def swapRelative = \x.
    if (x > 0) {
        currentItem <- grab;
        stride x;
        otherItem <- grab;
        place currentItem;
        turn back;
        stride x;
        place otherItem;
        turn back;
    } {};
    end;

/**
  Fisher-Yates shuffle on a physical array

  Precondition:
  * Array is oriented horizontally
  * Robot is placed at the head (left end) of the array
  * Robot is facing right

  "n" is the size of the array.
*/
def shuffle = \n. \i.
    if (i < n - 1) {
        let randAmplitude = n - i in
        x <- random randAmplitude;
        swapRelative x;
        move;
        shuffle n $ i + 1;
    } {};
    end;

// Inner loop in inversion-counting algorithm
def countInnerInversions = \n. \referenceVal. \j.

    if (j < n - 1) {
        move;
        valueHere <- getValueHere;
        let addend = if (referenceVal > valueHere) {1} {0} in
        recursiveSum <- countInnerInversions n referenceVal $ j + 1;
        let foo = recursiveSum in
        return $ addend + foo;
    } {
        return 0;
    };
    end

/**
  "n" represents array length.
  Runs in O(n^2) time.
*/
def countInversions = \n. \i.
    if (i < n - 1) {
        valueHere <- getValueHere;
        innerCount <- countInnerInversions n valueHere i;
        let innerCountFoo = innerCount in
        turn back;
        // Go backward one fewer space each time.
        stride $ n - i - 2;
        turn back;
        subarrayInversions <- countInversions n $ i + 1;
        let foo = subarrayInversions in
        return $ innerCountFoo + foo;
    } {
        return 0;
    };
    end

/**
Left is a Boolean indicating whether the tile has been drilled.
Right is a valid tile entity name.
*/
def scanValid : dir -> cmd (bool + text) = \d.
    maybeTileForward <- scan d;
    case maybeTileForward
        (\_. return $ inL false)
        (\x.
            if (x == "sliding-tile") {
                return $ inL true;
            } {
                y <- getOrdinal x;
                return $ if (y > 0) {
                    inR x;
                } {
                    inL false;
                };
            };
        );
    end

/**
Returns true if the (multi-row) search should terminate early.
*/
def findBlankInRow = \n.
    if (n > 0) {
        mt <- isempty;
        if mt {
            return true;
        } {
            move;
            findBlankInRow $ n - 1;
        };
    } {
        return false;
    };
    end;

def turnaround = \d.
    turn d;
    move;
    turn d;
    end;

/**
  Raster-scan the playfield
  until we get to an empty tile.
  If we don't find any, raise an exception
  that will end the game.
*/
def moveToBlankTile = \boardWidth.
    teleport self (0, 0);
    turn east;

    intersperseUntil 2 (turnaround left;) $
        intersperseUntil 2 (turnaround right) $
            findBlankInRow $ boardWidth - 1;
    end;

/**
  Precondition: The original item was a valid tile.
  Returns true if a drilling took place.
  Returns false if something unexpected happened
  and we should abort/reset.
*/
def actOnItemComparison = \maybeNewItem. \original.

    case maybeNewItem (\isSlidingTile.
        if isSlidingTile {
            create original;
            place original;
            move;
            grab;
            // Abort early from the recursion.
            return false;
        } {
            // The new tile is not a sliding tile.
            // We assume it's a blank tile and move there.
            // If it turns out not to be blank, that will
            // be addressed in the outer "observe" loop.
            move;
            return false;
        };
    ) (\newItem.
        let isSame = newItem == original in
        // We expect the tile to be unchanged, if it is not a sliding tile.
        if isSame {} {
            say $ "Original was " ++ original ++ "; newItem was " ++ newItem;
        };
        return isSame;
    );
    end;

def unwind = \keepChecking. \maybeItem.
    if keepChecking {

        turn right;

        // For now, we assume that there exist no "drilled" tiles
        // at the "wind-up"; the drilling shall always happen while
        // we are waiting at the peak of the recursion stack.
        maybeItem2 <- scanValid forward;

        keepGoing <- case maybeItem (
            \isSlidingTile. if isSlidingTile {
                // Our assumption was invalid; we don't have a
                // valid reference tile to compared the drilled tile to.
                say "Unexpected drilling; no reference tile.";
                return false;
            } {
                return true;
            }
        ) (actOnItemComparison maybeItem2);
        return keepGoing;
    } {
        return false;
    };
    end;

/** Precondition:
  Robot resides on the single blank tile within the puzzle rectangle.
  Our initial orientation does not matter.

  Strategy:
  Observe the entity contents in four directions, winding them into the stack.
  Then wait for a change to any of those cells.
  Upon change, unwind the stack, comparing the previously-known entity in each
  direction to the current entity.

  If there was no change, which can happen due to timeout of the `wait`,
  just wind up the stack again.
*/
def auditNeighbors = \depth.
    if (depth > 0) {
        maybeItemBlah <- scanValid forward;

        // NOTE: This let-binding circumvents bug #1032
        let maybeItem = maybeItemBlah in
        turn left;
        keepChecking <- auditNeighbors $ depth - 1;
        instant $ unwind keepChecking maybeItem;
    } {
        instant $ watchDir 4;
        wait 10000;
        return true;
    };
    end;

def getIndexesTotal = \n.
    if (n > 0) {
        let idx = n - 1 in
        teleport self (idx/4, -(mod idx 4));
        valueHere <- getValueHere;

        // This reassignment has to happen before the
        // recursive call due to #1032
        let valueHereBlah = valueHere in
        runningTotal <- getIndexesTotal $ n - 1;
        return $ valueHereBlah + runningTotal;
    } {
        return 0;
    }
    end;

/** If we iterate over all of the tiles, assigning each a contiguous index
starting with one, we can determine whether a single tile is missing
by subtrating the observed sum of indices from the expected sum.
*/
def findMissingIndex = \indicesSum.
    mySum <- getIndexesTotal 16;
    return $ indicesSum - mySum;
    end;

def getLetterEntityByIndex = \idx.
    let letter = toChar $ idx - 1 + charAt 0 "a" in
    letter ++ "-tile";
    end;

def teleportToDetectResult = \referenceLoc. \relativeLoc.
    let newLoc = sumTuples referenceLoc relativeLoc in
    teleport self newLoc;
    end;

/**
If the player has "drilled" a location that doesn't make
sense to move, find it and restore it to its original value.

Preconditions:
* We have already attempted to move a "sensibly"-marked tile.
* We are located at the bottom-left corner of the board.
*/
def findNonsensicalMarker = \indicesSum.
    detectReferenceLoc <- whereami;
    result <- detect "sliding-tile" ((0, 0), (3, 3));
    case result (\_. return ()) (\badLoc.
        missingIdx <- findMissingIndex indicesSum;
        if (missingIdx > 0) {
            // Fix it:
            teleportToDetectResult detectReferenceLoc badLoc;
            let entName = getLetterEntityByIndex missingIdx in
            create entName;
            _ <- swap entName;
            return ();
        } {};
    );
    end;

def observe = \indicesSum. \boardWidth.
    // We expect to begin each iteration on an empty tile.
    // If we are not, reposition ourselves.
    emptyHere <- isempty;
    if emptyHere {} {
        // Will return false
        moveToBlankTile boardWidth;
        return ();
    };

    try {
        auditNeighbors 4;
        return ();
    } {};

    atLocation (0, -3) $
        findNonsensicalMarker indicesSum;
    end;

def observeRec = \indicesSum. \boardWidth. \boardHeight.
    observe indicesSum boardWidth;

    // For efficiency; otherwise we will loop a large number of times each tick
    // (until a certain instruction-per-tick limit is reached)
    // wait 1;

    observeRec indicesSum boardWidth boardHeight;
    end;

def fixInversions = \arrayLoc. \arrayLength.
    teleport self arrayLoc;
    inversionCount <- countInversions arrayLength 0;

    if (isEven inversionCount) {} {
        teleport self arrayLoc;
        swapRelative 1;
    };
    end;

def prepareArray = \arrayLoc. \boardWidth. \boardHeight.
    
    teleport self arrayLoc;

    let cellCount = boardWidth * boardHeight in
    let arrayLength = cellCount - 1 in

    shuffle arrayLength 0;
    fixInversions arrayLoc arrayLength;
    end;

def relocateEnt = \from. \to.
    teleport self from;
    emptyHere <- isempty;
    if emptyHere {} {
        e <- grab;
        teleport self to;
        place e;
    };
    end;

def placeSingleRow = \sourceRow. \boardWidth. \rowIndex. \colIndex.
    if (colIndex >= 0) {
        relocateEnt (rowIndex*boardWidth + colIndex, sourceRow) (colIndex, -rowIndex);
        placeSingleRow sourceRow boardWidth rowIndex $ colIndex - 1;
    } {};
    end;

def placeRandomizedPuzzle = \arrayLoc. \boardWidth. \rowIndex.
    if (rowIndex >= 0) {
        placeSingleRow (snd arrayLoc) boardWidth rowIndex $ boardWidth - 1;
        placeRandomizedPuzzle arrayLoc boardWidth $ rowIndex - 1;
    } {};
    end;

def setupGame = \boardWidth. \boardHeight.
    let arrayLoc = (0, -6) in
    prepareArray arrayLoc boardWidth boardHeight;
    placeRandomizedPuzzle arrayLoc boardWidth $ boardHeight - 1;

    teleport self (4, -4);

    // Sentinel to indicate we are ready to start checking goal condition
    create "flower";
    end;

def go =
    let boardWidth = 4 in
    let boardHeight = 4 in
    instant $ setupGame boardWidth boardHeight;
    observeRec 120 boardWidth boardHeight;
    end;

go;