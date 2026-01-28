import "~swarm/lib/control"
import "~swarm/lib/arith"
import "~swarm/lib/tuple"

def getRelativeLocation = \absCurrentLoc. \absDestLoc.
    let negatedLoc = negateTuple absCurrentLoc in
    pure $ sumTuples negatedLoc absDestLoc;
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

def moveTuple = λmatch \x. \y.
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
    case maybeItem pure (\item.
        hasSome <- has item;
        harvest;
        if hasSome {} {
            while isempty {wait 1};
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
        pure tally;
    } {
        move;
        countLine $ tally + 1;
    }
    end;

def placeFinalCopy = \item.
    originalLoc <- whereami;
    match originalLoc \x. \_.
    goToLocation originalLoc (x, 0);
    until isempty move;
    place item;
    newLoc <- whereami;
    goToLocation newLoc originalLoc;
    end;

def copyIfNeeded = \targetCount.
    maybeItem <- scan down;
    case maybeItem pure (\item.
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
