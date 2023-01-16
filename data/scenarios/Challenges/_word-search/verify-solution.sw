/**
Algorithm:
We only need to check the base's
current position: if we find three contiguous highlights,
then we know that the player has just completed their
third highlight.
*/

def whichOrdinal =
    isC <- ishere "lowercase c";
    if (isC) {
        return 0;
    } {
        isO <- ishere "lowercase o";
        if (isO) {
            return 1;
        } {
            isW <- ishere "lowercase w";
            if (isW) {
                return 2;
            } {
                return (-1);
            }
        }
    }
    end;


def whichHighlightedOrdinal = \str.
    if (str == "lowercase c") {
        return 0;
    } {
        if (str == "lowercase o") {
            return 1;
        } {
            if (str == "lowercase w") {
                return 2;
            } {
                return (-1);
            }
        }
    }
    end;

def countConsecutive = \expectedOrdinal. \n.

    thisOrdinal <- whichOrdinal;
    nextOrdinal <- if (thisOrdinal == expectedOrdinal) {
        return $ expectedOrdinal + 1;
    } {
        return 0;
    };

    if (nextOrdinal == 3) {
        return true;
    } {
        if (n > 1) {
            move;
            countConsecutive nextOrdinal $ n - 1;
        } {
            return false;
        };
    };
    end;

def checkBackAndForth =

    foundBackward <- countConsecutive 0 3;
    if (foundBackward) {
        return true;
    } {
        turn back;
        countConsecutive 0 3;
    }
    end;

def checkDirections = \n.
    if (n > 0) {
        wasFound <- checkBackAndForth;
        if wasFound {
            return true;
        } {
            turn left;
            checkDirections $ n - 1;
        }
    } {
        return false;
    }
    end;

def isMarkedInDirection = \d.
    scanResult <- scan d;
    ordinalNum <- case scanResult
        (\_. return (-1))
        whichHighlightedOrdinal;
    return $ ordinalNum >= 0;
    end;

/**
It's possible we could be one cell away from
a marked cell after finishing, either due
to using a directional `drill` command instead of
`drill down`, or due to an apparent bug which
does not evaluate the goal condition between the
`drill` and a `move` command.
*/
def moveToMarkedCell = \n.
    if (n > 0) {
        isMarkedAhead <- isMarkedInDirection forward;
        if isMarkedAhead {
            move;
            return true;
        } {
            turn left;
            moveToMarkedCell $ n - 1;
        };
    } {
        return false;
    };
    end;

/**
Orient ourselves such that
a marked cell is behind us.
*/
def findMarkBehind = \n.
    if (n > 0) {
        isMarkedBehind <- isMarkedInDirection back;
        if isMarkedBehind {
            return true;
        } {
            turn left;
            findMarkBehind $ n - 1;
        };
    } {
        return false;
    };
    end;

/**
The cell we're on might be in the middle of a word,
rather than the end. Determine the orientation of
the line, then move along it until reaching the end.

Algorithm:
0. Assumption: we are currently on a marked cell.
1. Turn in all all four directions to `scan back`
   for a second marked cell. Stop turning if
   we encounter one.
   If none found after 4 turns, abort.
2. `scan forward` to see if there is a marked cell in
   the opposite direction.
   `move` (foward) once if there is. Since the word
   is only three cells long, this will be the other
   end of it.
*/
def moveToWordExtrema =
    foundCellBehind <- findMarkBehind 4;
    if foundCellBehind {
        isMarkedAhead <- isMarkedInDirection forward;
        if isMarkedAhead {
            move;
        } {};
    } {};
    end;

def checkSoln =
    isMarkedHere <- isMarkedInDirection down;
    atMarkedCell <- if isMarkedHere {
        return true;
    } {
        moveToMarkedCell 4;
    };

    if atMarkedCell {
        moveToWordExtrema;
        checkDirections 4;
    } {
        return false;
    }
    end;

as base {checkSoln};