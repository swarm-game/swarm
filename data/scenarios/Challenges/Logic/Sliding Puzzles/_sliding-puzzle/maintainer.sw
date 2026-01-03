/**
The "maintainer" bot handles legal and illegal moves.
*/

import "~swarm/lib/control"
import "~swarm/lib/arith"
import "~swarm/lib/tuple"
import "common"

/**
Sums of consecutive integers
*/
def computeTriangularNumber = \n.
    (n * (n + 1)) / 2
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
    pure retval;
    end;

def teleportToDetectResult = \referenceLoc. \relativeLoc.
    let newLoc = sumTuples referenceLoc relativeLoc in
    teleport self newLoc;
    end;

def getIndexesTotal = \boardWidth. \boardHeight. \n.
    if (n > 0) {
        let idx = n - 1 in
        teleport self (idx/boardHeight, -(mod idx boardWidth));
        valueHere <- getValueHere;
        runningTotal <- getIndexesTotal boardWidth boardHeight $ n - 1;
        pure $ valueHere + runningTotal;
    } {
        pure 0;
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
    pure $ indicesSum - mySum;
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
        case result (\_. handleLegalMove tileIdx; pure true;) (\_.
            turn left;
            hasAdjacentBlank tileIdx $ n - 1;
        );
    } {
        pure false;
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
    case result pure (\badLoc.
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

/**
  Recurses over all cells in all rows.
  Traverses within rows via physical `move`-ment.
  Wraps to the next row via teleport if a border is encountered.
  Terminates if still on a border immediately after wrapping.

  Precondition: Facing east at location (0, 0).
*/
def iterateAllTiles : Cmd Unit -> Cmd Unit = \func.
    let b = "border" in
    isOnBottomBorder <- itemIsHere b;
    if isOnBottomBorder {} {

        func;
        move;

        isOnRightBorder <- itemIsHere b;
        if isOnRightBorder {
            loc <- whereami;
            match loc \_. \y.
            teleport self (0, y - 1);
        } {};
        iterateAllTiles func;
    }
    end;

def watchBoard =
    turn east;
    atLocation (0, 0) $ iterateAllTiles $ (
        // Note: W actually don't need to watch the "empty" space
        watch down;
    );
    wait 1000;
    end;

def go = \boardWidth. \boardHeight.

    // Re-position at the bottom-left corner
    instant { atLocation (0, -(boardHeight - 1)) (
        handleMarker boardWidth boardHeight;

        // NOTE: I originally intended to use 'watch' as a performance optimization.
        // Hoewver, Using 'watch' seems to incur some lag in the 'maintainer' bot's reaction
        // such that the player is not replentished with 'ink' by the time they might
        // want to perform their next 'drill' operation.
        // watchBoard;
    )};

    // Throttle the recursion, otherwise it will max out the allowed operations per tick
    wait 1;

    go boardWidth boardHeight;
    end;

def maintain =
  until (has "flower") $ wait 1;
  go 3 3;
end
