/**
Note: we are being a bit devious here;
There actually will never be a horizontal solution.

We know epirically that our chosen seed does
contain at least one vertical solution.
*/
def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

/**
Call a function repeatedly with the numeric argument,
in descending order.
*/
def iterN = \n. \f.
    if (n > 0) {
        let newNum = n - 1 in
        f newNum;
        iterN newNum f;
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

To ensure there are limited numbers of solutions
(TODO: preferably exactly one),
make sure we're not completing a word
horizontally (foward or backward)
(TODO: except if we
are in the pre-designated location).

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
    currentLoc <- whereami;
    teleport self (0, snd currentLoc - 1);
    end;

/**
Recursive. Tracks the completion of the word in both the forward
and backward directions.
*/
def layTilesRow = \expectedFwdOrdinal. \expectedBkwdOrdinal. \n.
    placedIndex <- singleTile expectedFwdOrdinal expectedBkwdOrdinal;

    if (n > 1) {
        move;

        newFwdOrdinal <- if (placedIndex == expectedFwdOrdinal || placedIndex == 0) {
            return $ placedIndex + 1;
        } {
            return 0;
        };

        newBkwdOrdinal <- if (placedIndex == expectedBkwdOrdinal || placedIndex == 2) {
            return $ placedIndex - 1;
        } {
            return 2;
        };

        layTilesRow newFwdOrdinal newBkwdOrdinal $ n - 1;
    } {};
    end;

def giveLetterNumbered = \n.
    letter <- chooseLetter n;
    give base letter;
    end;

def removeBoulder =
    baseLoc <- as base {whereami};
    teleport self (fst baseLoc - 1, snd baseLoc);

    // Remove the boulder blocking the player's path
    grab;

    // Make sure the base "knows" about the letters
    // to get rid of the question marks ("?").
    iterN 3 giveLetterNumbered;

    selfdestruct;
    end;

def createPuzzle = \width. \height.
    intersperse height (crossBack width) (layTilesRow 0 2 width);
    removeBoulder;
    end;

createPuzzle 25 15;