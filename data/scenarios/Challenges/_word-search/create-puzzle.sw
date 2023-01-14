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
Calls "whichOrdinal" on the cell to the north.
Returns -1 if not a recognized letter.
*/
def getAdjacentOrdinal = \d.
    foo <- scan d;
    str <- case foo (\_. return "") (\s. return s);
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
(TODO: preferably exactly one),
make sure we're not completing a word
horizontally (foward or backward)
or vertically (upward or downward), except if we
are in the pre-designated location).

If we would be completing
a word, select a different random letter.
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
Recurse for each cardinal direction
*/
def isInsertionCandidate = \n.
    if (n > 0) {
        leftOrd <- getAdjacentOrdinal west;
        rightOrd <- getAdjacentOrdinal east;
        if (leftOrd == 0 && rightOrd == 2) {
            return true;
        } {
            turn left;
            isInsertionCandidate $ n - 1;
        };
    } {
        return false;
    }
    end;

def tryRowInsertion = \rowWrapCount. \n.
    if (n > 0) {
        canPlaceHere <- isInsertionCandidate 4;
        if canPlaceHere {
            chosenLetter <- chooseLetter 1;
            swap chosenLetter;
            return true;
        } {
            if (n == rowWrapCount) {
                currentLoc <- whereami;
                teleport self (0, snd currentLoc);
            } {
                move;
            };
            tryRowInsertion rowWrapCount $ n - 1;
        }
    } {
        return false;
    }
    end;

/**
Iterates over tryRowInsertion for the whole puzzle.
*/
def tryPuzzleInsertion = \rowWrapCount. \width. \colWrapCount. \n.
    if (n > 0) {
        didFinish <- tryRowInsertion rowWrapCount width;
        if didFinish {
            return true;
        } {
            currentLoc <- whereami;
            if (n == colWrapCount) {
                teleport self (fst currentLoc, 0);
            } {
                teleport self (fst currentLoc, snd currentLoc - 1);
            };

            tryPuzzleInsertion rowWrapCount width colWrapCount $ n - 1;
        }
    } {
        return false;
    }
    end;

/**
Assumption: this location is padded by
one cell from the vertical and horizontal
edges of the playfield.
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
    iterN 3 $ \x.
        chosenLetter <- chooseLetter x;
        swap chosenLetter;
        move;
    end;

/**
First, choose a random location to start.
Then perform a raster scan (which wraps around
back to the starting location), looking for
any location where insertion of an O
will complete a word. In other words, one
of these four possibilities:

xCx  xxx  xWx  xxx
xxx  CxW  xxx  WxC
xWx  xxx  xCx  xxx

which, one may notice, are rotationally symmetric.
Inserting an O in the center cell will result in 
either one or two solutions being created.

In the unlikely event that this scan completes without
finding such an arrangement, the complete word
shall be written at a random location and orientation
irrespective of the existing cell contents.
*/
def addSolutionToPuzzle = \width. \height.

    startX <- random width;
    startY <- random height;

    // Proceed from left to right, top to bottom.
    teleport self (startX, -startY);
    turn east;

    // Note: this has indeterminate runtime and can be slow:
    wasInserted <- tryPuzzleInsertion (startX + 1) width (startY + 1) height;

    if wasInserted {} {
        overwriteWithWord width height;
    }
    end;

def createPuzzle = \width. \height.
    createImpossiblePuzzle width height;
    addSolutionToPuzzle width height;
    removeBoulder;
    end;

createPuzzle 25 15;