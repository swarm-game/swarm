def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
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

/**
We may have selected the last letter to complete
the word COW.

To ensure there is only exactly one solution,
make sure we're not completing a word
horizontally (foward or backward) or
vertically (forward or backward), except if we
are in the pre-designated location.

If we would be completing
a word, select a different random letter.
*/
def reRoll = \expectedFwdOrdinal. \expectedBkwdOrdinal.

    letterIndex <- random 3;
    let completingFwd = letterIndex == expectedFwdOrdinal && expectedFwdOrdinal == 2 in
    let completingBkwd = letterIndex == expectedBkwdOrdinal && expectedBkwdOrdinal == 0 in
    if (completingFwd || completingBkwd) {
        if (completingFwd && completingBkwd) {
            return 1;
        } {
            reRoll expectedFwdOrdinal expectedBkwdOrdinal;
        };
    } {
        return letterIndex;
    };

    end;

def singleTile = \expectedFwdOrdinal. \expectedBkwdOrdinal.

    letterIndex <- reRoll expectedFwdOrdinal expectedBkwdOrdinal;
    chosenLetter <- chooseLetter letterIndex;
    place chosenLetter;
    return letterIndex;
    end;

def crossBack = \n.
    turn right;
    move;
    turn right;
    doN (n - 1) move;
    turn back;
    end;

/**
Recursive. Tracks the completion of the word in both the forward
and backward directions.
*/
def layTilesRow = \expectedFwdOrdinal. \expectedBkwdOrdinal. \n.
    placedIndex <- singleTile expectedFwdOrdinal expectedBkwdOrdinal;

    if (n > 1) {
        move;

        newFwdOrdinal <- if (placedIndex == expectedFwdOrdinal && expectedFwdOrdinal != 2) {
            return $ expectedFwdOrdinal + 1;
        } {
            return 0;
        };

        newBkwdOrdinal <- if (placedIndex == expectedBkwdOrdinal && expectedBkwdOrdinal != 0) {
            return $ expectedBkwdOrdinal - 1;
        } {
            return 2;
        };

        layTilesRow newFwdOrdinal newBkwdOrdinal $ n - 1;
    } {};
    end;

def removeBoulder =
    turn left;
    doN 4 move;
    turn right;
    move;

    // Remove the boulder blocking the player's path
    grab;
    end;

def createPuzzle = \width. \height.
    intersperse height (crossBack width) (layTilesRow 0 2 width);
    removeBoulder;
    end;

createPuzzle 25 15;