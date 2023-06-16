/**
This robot is responsible for both the initial board setup.
*/

def id = \t. t end
def elif = \t. \then. \else. {if t then else} end
def else = id end

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def mod : int -> int -> int = \i.\m.
  i - m * (i / m)
end

def isEven = \n.
    mod n 2 == 0;
    end

/** One-based index
*/
def getLetterEntityByIndex = \idx.
    let letter = toChar $ idx - 1 + charAt 0 "a" in
    letter ++ "-tile";
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

/**
If there is an odd number of inversions, simply
swap the first two tiles to obtain the correct
parity.
*/
def fixInversions = \arrayLoc. \arrayLength.
    teleport self arrayLoc;
    inversionCount <- countInversions arrayLength 0;
    if (isEven inversionCount) {} {
        teleport self arrayLoc;
        swapRelative 1;
    };
    end;

def layOrderedTiles = \n.
    if (n > 0) {
        let tileName = getLetterEntityByIndex n in
        create tileName;
        place tileName;
        move;
        layOrderedTiles $ n - 1;
    } {};
    end;

def prepareArray = \arrayLoc. \boardWidth. \boardHeight.
    
    turn east;
    teleport self arrayLoc;

    layOrderedTiles $ (boardWidth * boardHeight) - 1;

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

def drawBorderLine = \boardSideLength.
    let b = "border" in
    doN (boardSideLength + 1) $ (
        create b;
        place b;
        move;
    );
    turn right;
    end;

def createBorder = \boardWidth. \boardHeight.
    teleport self (-1, 1);

    turn east;
    doN 2 (
        drawBorderLine boardWidth;
        drawBorderLine boardHeight;
    );
    end;

def setupGame = \boardWidth. \boardHeight.
    createBorder boardWidth boardHeight;
    let arrayLoc = (0, -6) in
    prepareArray arrayLoc boardWidth boardHeight;
    placeRandomizedPuzzle arrayLoc boardWidth $ boardHeight - 1;

    teleport self (6, -6);

    // Sentinel to indicate we are ready to start checking goal condition
    r <- robotnamed "maintainer";
    give r "flower";
    end;

def go = \boardWidth. \boardHeight.
    instant $ setupGame boardWidth boardHeight;
    end;

go 3 3;