def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def whichOrdinal = \str.
    if (str == "capital C") {
        return 0;
    } {
        if (str == "capital O") {
            return 1;
        } {
            if (str == "capital W") {
                return 2;
            } {
                return (-1);
            }
        }
    }
    end;

/**
Returns -1 if not a recognized letter.
*/
def getAdjacentOrdinal = \d.
    maybeEntity <- scan d;
    str <- case maybeEntity (\_. return "") (\s. return s);
    whichOrdinal str;
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
We only need to probe further if
the letter above us is an O.
Then we need to check whether the letter
above that is a C or a W.
*/
def getExcludedVerticalLetter =
    northOrdinal <- getAdjacentOrdinal north;
    if (northOrdinal == 1) {
        currentLoc <- whereami;
        teleport self (fst currentLoc, snd currentLoc + 1);
        doubleNorthOrdinal <- getAdjacentOrdinal north;
        teleport self currentLoc;

        if (doubleNorthOrdinal == 2) {
            return 0;
        } {
            if (doubleNorthOrdinal == 0) {
                return 2;
            } {
                return (-1);
            }
        }
    } {
        return (-1);
    }
    end;

/**
To ensure there are limited numbers of solutions
(preferably exactly one),
make sure we're not completing a word
horizontally (foward or backward)
or vertically (upward or downward), except if we
are in the designated location.
*/
def reRoll = \excludedVertical. \expectedFwdOrdinal. \expectedBkwdOrdinal.

    // NOTE: excludeTwo and excludeZero
    // are mutually exclusive!
    let excludeTwo = expectedFwdOrdinal == 2 in
    let excludeZero = expectedBkwdOrdinal == 0 in

    // NOTE: Excluded letters can only be 0 or 2.
    // 1 is always a valid option.

    if excludeZero {
        if (excludedVertical == 2) {
            return 1;
        } {
            // Zero is the only excluded value,
            // so just offset a choice between 0 and 1 upward by 1, 
            // to make it a choice between 1 and 2.
            val <- random 2;
            return $ val + 1;
        };
    } {
        if excludeTwo {
            if (excludedVertical == 0) {
                return 1;
            } {
                // Two is the only excluded value,
                // so make it a choice between 0 and 1.
                random 2;
            };
        } {
            if (excludedVertical == 0) {
                // Zero is the only excluded value,
                // so just offset a choice between 0 and 1 upward by 1, 
                // to make it a choice between 1 and 2.
                val <- random 2;
                return $ val + 1;
            } {
                if (excludedVertical == 2) {
                    // Two is the only excluded value,
                    // so make it a choice between 0 and 1.
                    random 2;
                } {
                    // No values are excluded, so select random from
                    // the full range.
                    random 3;
                }
            };
        };
    };
    end;

def singleTile = \expectedFwdOrdinal. \expectedBkwdOrdinal.
    excludedVertical <- getExcludedVerticalLetter;
    letterIndex <- reRoll excludedVertical expectedFwdOrdinal expectedBkwdOrdinal;
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

def createImpossiblePuzzle = \width. \height.
    intersperse height (crossBack width) (layTilesRow 0 2 width);
    end;

/**
This word center is padded by
one cell from the vertical and horizontal
edges of the playfield.
This means that the word will not exist
horizontally at either the top or bottom edge,
nor will it exist vertically at the left or
right edge of the playfield.
*/
def overwriteWithWord = \width. \height.

    randX <- random $ width - 2;
    randY <- random $ height - 2;
    let insertionX = 1 + randX in
    let insertionY = 1 + randY in
    teleport self (insertionX, -insertionY);

    // Rotate to random orientation
    turnCount <- random 3;
    doN turnCount $ turn left;

    move;
    turn back;

    // Place all three letters of the word
    iterN 3 $ \x.
        chosenLetter <- chooseLetter x;
        swap chosenLetter;
        move;
    end;

def createPuzzle = \width. \height.
    createImpossiblePuzzle width height;
    overwriteWithWord width height;
    removeBoulder;
    end;

instant $ createPuzzle 25 15;