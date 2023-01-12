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
        return 0;
    } {
        isO <- ishere "capital O";
        if (isO) {
            return 1;
        } {
            isW <- ishere "capital W";
            if (isW) {
                return 2;
            } {
                return (-1);
            }
        }
    }
    end;

// Go to upper-left corner
def goToCorner =
    myLoc <- whereami;
    doN (fst myLoc) move;
    turn right;
    doN (-(snd myLoc)) move;
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
        return $ theFoundOrdinal + 1;
    } {
        // Reset the progress
        return 0;
    };

    if (newExpectedOrdinal == 3) {
        turn back;

        intersperse 3 move highlightLetter;
        return true;
    } {
        if (colCount > 1) {
            move;
            traverseRow newExpectedOrdinal (colCount - 1);
        } {
          return false;
        };
    };
    end;


def advanceRow =
    turn left;
    move;
    turn left;
    end;

/**
Visit all rows, then
visit all columns.
*/
def traverseCols = \width. \height. 
    didWin <- traverseRow 0 width;
    if didWin {
        return true;
    } {
        turn back;
        didWinBackward <- traverseRow 0 width;
        if didWinBackward {
            return true;
        } {
            if (height > 1) {
                advanceRow;
                traverseCols width $ height - 1;
            } {
                return false;
            };
        }
    } 
    end;

def solve =
    waitUntilUnblocked;
    goToCorner;

    wonHorizontally <- traverseCols 25 15;
    if wonHorizontally {
        return true;
    } {
        turn right;
        traverseCols 15 25;
    }
    end;

solve;
