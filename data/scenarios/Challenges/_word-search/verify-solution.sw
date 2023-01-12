// Traverse all rows, left-to-right, to see if the highlighted letters
// exist in the proper order.
// Fails if more than 3 letters are highlighted.


def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def boolToNum = \b.
    if b {return 1} {return 0};
    end;

def resetBits =
    countOnes <- count "bit (1)";
    doN countOnes $ make "bit (0)";
    end;

// Counts how many times a predicate was true in a loop.
// NOT USED
def countInRow = \act. \pred. \n.
    if (n > 0) {
        isTrue <- pred;
        if isTrue {
            make "bit (1)";
        } {};

        act;
        countInRow act pred $ n - 1;
    } {};
    end;


def chooseLetter = \i.
    if (i == 0) {
        return "lowercase c";
    } {
        if (i == 1) {
            return "lowercase o";
        } {
            return "lowercase w";
        }
    };
    end;


// NOT USED
def isHighlighted =
    isC <- ishere "lowercase c";
    if (isC) {
        return true;
    } {
        isO <- ishere "lowercase o";
        if (isO) {
            return true;
        } {
            isW <- ishere "lowercase w";
            return isW;
        }
    }
    end;


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


// NOT USED
def countHighlighted = \n.
    countInRow move isHighlighted n;
    hCount <- count "bit (1)";
    return hCount;
    end;

def moveToRowBeginning = \r.
    currentPos <- as r {whereami};
    teleport r (0, snd currentPos);
    end;


/** Returns a tuple:
* (didFindWord, isSolutionInvalid)
*
* The necessary condition to win the scenario is
* [didFindWord == true] for ANY row,
* and
* [isSolutionInvalid == false] for ALL rows.
*
* We may need to check all of the rows, though
* we can abort early with failure if any row has
* [isSolutionInvalid == false]
*/
def traverseRow = \isWordFound. \highlightCount. \expectedOrdinal. \n.

    if (n > 0) {

        observedOrdinal <- whichOrdinal;

        let isHighlighted = observedOrdinal >= 0 in

        newHighlightCount <- if isHighlighted {
            return $ highlightCount + 1;
        } {
            return highlightCount;
        };

        nextExpectedOrdinal <- if (observedOrdinal == expectedOrdinal) {
            return $ expectedOrdinal + 1;
        } {
            // Zero progress.

            // TODO: we have the potential to short-circuit here,
            // if (expectedOrdinal > 0). Resetting after having
            // made some progress implies overall failure,
            // because a valid solution has no discontinuities.

            // Note that examining the vertical direction (columns)
            // could still yield a win.
            return 0;
        };

        // This boolean function argument is a latch.
        let newIsWordFound = isWordFound || (nextExpectedOrdinal == 3) in

        move;
        traverseRow newIsWordFound newHighlightCount nextExpectedOrdinal (n - 1);

    } {
        // Done recursing, have exhausted the row.
        return (isWordFound, highlightCount);
    };
    end;

def traverseAllRows = \r. \didFindWord. \highlightCount. \n.

    if (n > 0) {
            
        foundWordAndHighlightCount <- traverseRow didFindWord highlightCount 0 25;
        // first element: found word
        // second element: highlight count

        let newDidFindWord = fst foundWordAndHighlightCount in
        let newHighlightCount = snd foundWordAndHighlightCount in

        if (newHighlightCount > 3) {
            return false;
        } {
            moveToRowBeginning r;
            resetBits;
            traverseAllRows r newDidFindWord newHighlightCount $ n - 1;
        };
    } {
        return didFindWord;
    }
    end;

def checkSoln =

    r <- robotnamed "lettersetter";

    as r {
        finishedPlacing <- has "boulder";

        if (finishedPlacing) {

            teleport r (0, 0);

            traverseAllRows r false 0 15;

        } {
            return false;
        };
    };
    end;

checkSoln;
