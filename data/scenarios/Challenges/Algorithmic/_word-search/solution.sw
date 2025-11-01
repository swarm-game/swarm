def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def waitUntilUnblocked =
    x <- blocked;
    if x {
        wait 1;
        waitUntilUnblocked;
    } {};
    end;

def whichOrdinal =
    isC <- ishere "capital C";
    if (isC) {
        pure 0;
    } {
        isO <- ishere "capital O";
        if (isO) {
            pure 1;
        } {
            isW <- ishere "capital W";
            if (isW) {
                pure 2;
            } {
                pure (-1);
            }
        }
    }
    end;

// Go to upper-left corner
def goToCorner =
    myLoc <- whereami;
    match myLoc \x. \y.
    doN x move;
    turn right;
    doN (-y) move;
    turn right;
    end;

def highlightLetter =
    drill down;
    end;

def traverseRow = \expectedOrdinal. \colCount.

    theFoundOrdinal <- whichOrdinal;

    // Logic: the first letter of the target word is *always*
    // considered a "match".
    let shouldAdvance = theFoundOrdinal == expectedOrdinal || theFoundOrdinal == 0 in
    newExpectedOrdinal <- if shouldAdvance {
        pure $ theFoundOrdinal + 1;
    } {
        // Reset the progress
        pure 0;
    };

    if (newExpectedOrdinal == 3) {
        turn back;

        intersperse 3 move highlightLetter;
        pure true;
    } {
        if (colCount > 1) {
            move;
            traverseRow newExpectedOrdinal (colCount - 1);
        } {
          pure false;
        };
    };
    end;

def advanceRow =
    turn left;
    move;
    turn left;
    end;

/**
Travels forward and then backward
across a row, to check for solutions
in either direction.
*/
def traverseCols = \width. \height. 
    didWin <- traverseRow 0 width;
    if didWin {
        pure true;
    } {
        turn back;
        didWinBackward <- traverseRow 0 width;
        if didWinBackward {
            pure true;
        } {
            if (height > 1) {
                advanceRow;
                traverseCols width $ height - 1;
            } {
                pure false;
            };
        }
    } 
    end;

def solve = \boardWidth. \boardHeight.
    waitUntilUnblocked;
    goToCorner;

    wonHorizontally <- traverseCols boardWidth boardHeight;
    if wonHorizontally {
        pure true;
    } {
        // If we did not find a horizontal solution,
        // look for vertical solutions.
        turn right;
        traverseCols boardHeight boardWidth;
    }
    end;

solve 25 15;
