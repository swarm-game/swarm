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

def chooseLetter = \i.
    if (i == 0) {
        return "capital C";
    } {
        if (i == 1) {
            return "capital O";
        } {
            return "capital W";
        }
    };
    end;

// Go to upper-left corner
def goToCorner =
    move;
    doN 25 move;
    turn right;
    doN 10 move;
    turn right;
    end;

def highlightLetter =
    drill down;
    end;

def traverseRow = \expectedOrdinal. \colCount.

    targetItem <- chooseLetter expectedOrdinal;
    targetHere <- ishere targetItem;
    log $ "Target: " ++ targetItem;
    log $ "Is here? " ++ format targetHere;

    newExpectedOrdinal <- if targetHere {
        return $ expectedOrdinal + 1;
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
    turn back;
    loc <- whereami;
    doN (fst loc) move;
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
        if (height > 1) {
            advanceRow;
            traverseCols width $ height - 1;
        } {
            return false;
        };
    } 
    end;

def solve =
    waitUntilUnblocked;
    goToCorner;

    traverseCols 25 15;
    end;

solve;
