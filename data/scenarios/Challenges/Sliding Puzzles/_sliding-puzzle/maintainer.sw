/**
The "maintainer" bot handles legal and illegal moves.
*/

/**
Sums of consecutive integers
*/
def computeTriangularNumber = \n.
    (n * (n + 1)) / 2
    end;

def mod : int -> int -> int = \i.\m.
    i - m * (i / m);
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

def getLetterEntityByIndex = \idx.
    let letter = toChar $ idx - 1 + charAt 0 "a" in
    letter ++ "-tile";
    end;

def teleportToDetectResult = \referenceLoc. \relativeLoc.
    let newLoc = sumTuples referenceLoc relativeLoc in
    teleport self newLoc;
    end;

def getOrdinal : text -> cmd int = \item.
    count $ item ++ "-ordinal";
    end;

def getValueHere =
    maybeItem <- scan down;
    ordNum <- case maybeItem (\_. return 0) getOrdinal;
    end;

def getIndexesTotal = \boardWidth. \boardHeight. \n.
    if (n > 0) {
        let idx = n - 1 in
        teleport self (idx/boardHeight, -(mod idx boardWidth));
        valueHere <- getValueHere;

        // This reassignment has to happen before the
        // recursive call due to #1032
        let valueHereBlah = valueHere in
        runningTotal <- getIndexesTotal boardWidth boardHeight $ n - 1;
        return $ valueHereBlah + runningTotal;
    } {
        return 0;
    }
    end;

/**
If we iterate over all of the tiles, assigning each a contiguous index
starting with one, we can determine whether a single tile is missing
by subtrating the observed sum of indices from the expected sum.
*/
def findMissingIndex = \boardWidth. \boardHeight.
    let squareCount = boardWidth * boardHeight in
    let tileCount = squareCount - 1 in
    let indicesSum = computeTriangularNumber tileCount in
    mySum <- getIndexesTotal boardWidth boardHeight squareCount;
    return $ indicesSum - mySum;
    end;

def replenishInk =
    baseHasInk <- as base {has "ink"};
    if baseHasInk {} {
        create "ink";
        give base "ink";
    };
    end;

def replaceTileByIndex = \idx.
    let entName = getLetterEntityByIndex idx in
    create entName;
    _ <- swap entName;
    replenishInk;
    end;

def placeTileByIndex = \idx.
    let entName = getLetterEntityByIndex idx in
    create entName;
    place entName;
    end;

def handleLegalMove = \tileIdx.
    grab;
    move;
    placeTileByIndex tileIdx;
    replenishInk;
    end;

/**
Checks in the four directions.
*/
def hasAdjacentBlank = \tileIdx. \n.
    if (n > 0) {
        result <- scan forward;
        case result (\_. handleLegalMove tileIdx; return true;) (\_.
            turn left;
            hasAdjacentBlank tileIdx $ n - 1;
        );
    } {
        return false;
    }
    end;

/**
Check whether the mark is adjacent to a blank space
*/
def isLegalMark = \tileIdx.
    hasAdjacentBlank tileIdx 4;
    end;

/**
If the player has "drilled" a location that doesn't make
sense to move, find it and restore it to its original value.

Preconditions:
* We have already attempted to move a "sensibly"-marked tile.
* We are located at the bottom-left corner of the board.
*/
def handleMarker = \boardWidth. \boardHeight.
    detectReferenceLoc <- whereami;
    result <- detect "sliding-tile" ((0, 0), (boardWidth - 1, boardHeight - 1));
    case result return (\badLoc.
        teleportToDetectResult detectReferenceLoc badLoc;
        missingIdx <- atLocation (0, 0) $ findMissingIndex boardWidth boardHeight;
        markIsLegal <- isLegalMark missingIdx;
        if markIsLegal {} {
            if (missingIdx > 0) {
                replaceTileByIndex missingIdx;
                say "Illegal move";
            } {};
        }
    );
    end;

def go = \boardWidth. \boardHeight.

    // Re-position at the bottom-left corner
    instant $ atLocation (0, -(boardHeight - 1)) $
        handleMarker boardWidth boardHeight;

    // Throttle the recursion, otherwise it will max out the allowed operations per tick.
    wait 1;

    go boardWidth boardHeight;
    end;

go 3 3;