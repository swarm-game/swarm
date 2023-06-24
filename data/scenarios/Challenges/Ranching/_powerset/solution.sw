def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def until = \p. \c. q <- p; if q {} {c; until p c} end;
def while = \p. until (x <- p; return $ not x) end;

def abs = \n. if (n < 0) {-n} {n} end;

def intersperse = \n. \f2. \f1.
    if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def mapTuple = \f. \t.
    (f $ fst t, f $ snd t)
    end;

def sumTuples = \t1. \t2.
    (fst t1 + fst t2, snd t1 + snd t2);
    end;

def negateTuple = \t.
    mapTuple (\x. -x) t;
    end;

def getRelativeLocation = \absCurrentLoc. \absDestLoc.
    let negatedLoc = negateTuple absCurrentLoc in
    return $ sumTuples negatedLoc absDestLoc;
    end;

def splitStride = \n.
    let dist = abs n in
    if (dist > 64) {
        stride 64;
        splitStride $ dist - 64;
    } {
        stride dist;
    }
    end;

def moveTuple = \tup.
    let x = fst tup in
    let y = snd tup in
    turn $ if (x > 0) {east} {west};
    // doN (abs x) move;
    splitStride x;

    turn $ if (y > 0) {north} {south};
    // doN (abs y) move;
    splitStride y;
    end;

def goToLocation = \currentLoc. \absoluteDestination.
    relativeDestination <- getRelativeLocation currentLoc absoluteDestination;
    moveTuple relativeDestination;
    end;

def recordFirstEncounter = \stashLoc. \item.
    originalHeading <- heading;
    originalLoc <- whereami;
    goToLocation originalLoc stashLoc;
    turn south;
    until isempty move;
    place item;

    newCurrentLoc <- whereami;
    goToLocation newCurrentLoc originalLoc;
    turn originalHeading;
    end;

def tryHarvest = \stashLoc.
    maybeItem <- scan down;
    case maybeItem return (\item.
        hasSome <- has item;
        harvest;
        if hasSome {} {
            while isempty $ wait 1;
            // Grab another one so that our "sentinel condition" won't
            // be invalidated when we go on to place it
            harvest;
            recordFirstEncounter stashLoc item;
        };
    );
    end;

def doRow = \stashLoc. \sandLength.
    intersperse (sandLength - 1) move $ tryHarvest stashLoc;
    end;

def turnaround = \d.
    turn d;
    move;
    turn d;
    end;

/**
Precondition:
At the start of the line, facing along the line.
*/
def countLine = \tally.
    emptyhere <- isempty;
    if emptyhere {
        turn back;
        splitStride tally;
        return tally;
    } {
        move;
        countLine $ tally + 1;
    }
    end;

def placeFinalCopy = \item.
    originalLoc <- whereami;
    goToLocation originalLoc (fst originalLoc, 0);
    until isempty move;
    place item;
    newLoc <- whereami;
    goToLocation newLoc originalLoc;
    end;

def copyIfNeeded = \targetCount.
    maybeItem <- scan down;
    case maybeItem return (\item.
        quantity <- count item;
        if (quantity < targetCount) {
            placeFinalCopy item;
        } {};
    );
    move;
    end;

def harvestForCounts = \rowLength. \stashLoc. \sweepCount.

    intersperse sweepCount (turnaround right) $
        intersperse 2 (turnaround left) $ doRow stashLoc rowLength;
    
    turnaround right;
    doRow stashLoc rowLength;
    end;

def go = \sweepCount.
    until (has "bell") $ wait 2;
    
    move;
    rowLength <- countLine 0;
    let stashLoc = (rowLength - 1, -2) in
    turnaround right;

    harvestForCounts rowLength stashLoc sweepCount;

    originalLoc <- whereami;
    goToLocation originalLoc stashLoc;
    turn south;

    entityCardinality <- countLine 0;

    turn back;
    let expectedCount = 2^(entityCardinality - 1) - 1 in
    doN entityCardinality $ copyIfNeeded expectedCount;

    // Mark goal-checkability sentinel
    place "bell";
    end;

go 3;