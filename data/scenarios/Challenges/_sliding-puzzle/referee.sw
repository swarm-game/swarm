/**
The "referee" bot detects illegal moves.

The "maintainer" robot sits on the blank space and observes neighbors for *legal* moves.
It utilizes the "watch" command for performance, but therefore cannot immediately
react to illegal (i.e. non-adjacent) marks.  Therfore, we have this "referee" bot
to "revert" illegal markings.
*/

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

/**
If we iterate over all of the tiles, assigning each a contiguous index
starting with one, we can determine whether a single tile is missing
by subtrating the observed sum of indices from the expected sum.
*/
def findMissingIndex = \indicesSum.
    mySum <- getIndexesTotal 16;
    return $ indicesSum - mySum;
    end;


def hasAdjacentBlank = \n.
    if (n > 0) {
        result <- scan forward;
        case result (\_. return true;) (\_.
            turn left;
            hasAdjacentBlank $ n - 1;
        );
    } {
        return false;
    }
    end;

/**
Check whether the mark is adjacent to a blank space
*/
def isLegalMark =
    hasAdjacentBlank 4;
    end;

def replenishInk =
    baseHasInk <- as base {has "ink"};
    if baseHasInk {} {
        create "ink";
        give base "ink";
    };
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
        teleportToDetectResult detectReferenceLoc badLoc;
        markIsLegal <- isLegalMark;
        if markIsLegal {} {
            missingIdx <- atLocation (0, 0) $ findMissingIndex indicesSum;
            if (missingIdx > 0) {
                let entName = getLetterEntityByIndex missingIdx in
                create entName;
                _ <- swap entName;
                say "Illegal move";
                replenishInk;
            } {};
        }
    );
    end;

def go =
    let indicesSum = 120 in
    instant $ atLocation (0, -3) $
        findNonsensicalMarker indicesSum;

    // Throttle the recursion, otherwise it will max out the allowed operations per tick.
    wait 1;

    go;
    end;

go;