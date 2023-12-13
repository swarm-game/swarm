/**
Generates several sequences of brackets, with some being invalid.
Remembers which rows that it generated are invalid, and loops back to check
the answer given by the player.

Note: to be an interesting challenge, there should be more than one
type of brackets. With just a single pair of brackets, the
sequence can be invalid if either of the following take place:
1) Different numbers of opening and closing brackets
2) The number of closing parentheses exceeds the number of opening parentheses
   at some point in the sequence.

Simply comparing the number of opening and closing brackets for equality
is too easy of a solution, so we would like to ensure that at least one
of the invalid solutions has an equal number of open and closing brackets but
goes "negative" in the balance at some point, so as to catch any solutions
that do not account for this "invalid state" within the sequence.

In contrast, with two types of brackets, a sequence can still be invalid
even if neither of the two conditions above take place; interleaved bracket pairs
are invalid, e.g. "({)}".
   
*/

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

// Not used:
def intersperse = \n. \f1. \f2.
    f1;
    if (n > 1) {
        f2;    
        intersperse (n - 1) f1 f2;
    } {};
    end;

def waitUntilOccupied =
    isUnoccupied <- isempty;
    if isUnoccupied {
        waitUntilOccupied
    } {};
    end;

def getRandomBool =
    dice <- random 2;
    if (dice == 1) {
        return true;
    } {
        return false;
    };
    end;

def bracketForBool = \isOpening.
    if isOpening {
        return "left bracket";
    } {
        return "right bracket";
    };
    end;

/**
Returns "inl ()" if the value is discretionary,
or "inr bool" if the value is mandatory.
*/
def getRequiredValue = \currentDepth. \remaining.
    if (currentDepth > 0) {
        if (currentDepth < remaining) {
            return $ inl ();
        } {
            return $ inr false;
        };
    } {
        return $ inr true;
    };
    end;

def makeMatchedBrackets = \mismatchIndex. \currentDepth. \remaining.

    reqVal <- getRequiredValue currentDepth remaining;
    shouldIncrease <- case reqVal (\_. getRandomBool) return;

    newDepth <- if shouldIncrease {
        return $ currentDepth + 1;
    } {
        return $ currentDepth - 1;
    };

    bracketIsOpening <- if (remaining == mismatchIndex) {
        return $ not shouldIncrease;
    } {
        return shouldIncrease;
    };

    bracketEntity <- bracketForBool bracketIsOpening;
    place bracketEntity;

    if (remaining > 1) {
        move;
        makeMatchedBrackets mismatchIndex newDepth $ remaining - 1;
    } {};
    end;

def returnToLeftSide = \loc.
    teleport self (fst loc, snd loc - 1);
    end;

/**
0 = False
1 = True
*/
def getBitTruth = \item.
    if (item == "bit (1)") {
        return $ inr true;
    } {
        if (item == "bit (0)") {
            return $ inr false;
        } {
            return $ inl ();
        };
    };
    end;

def makeRows = \size. \rowCount.

    currentLoc <- whereami;

    makeValidRowTemp <- getRandomBool;
    let makeValidRow = makeValidRowTemp in

    // TODO Ensure that invalid rows still
    // have an equal number of left and right brackets
    mismatchIndex <- if makeValidRow {
        return (-1);
    } {
        random size;
    };

    makeMatchedBrackets mismatchIndex 0 size;

    recursiveSuccessTemp <- if (rowCount > 1) {
        returnToLeftSide currentLoc;
        makeRows size $ rowCount - 1;
    } {
        // Waits to verify the solution...
        say "ready";
        move;
        turn left;
        waitUntilOccupied;
        return true;
    };

    let recursiveSuccess = recursiveSuccessTemp in

    itemHere <- scan down;
    isTrueBit <- case itemHere (\x. return $ inl x) getBitTruth;

    successNow <- case isTrueBit (\_. say "Missing result!"; return false;) (\truth.
        return $ makeValidRow == truth;
    );
    move;
    return $ successNow && recursiveSuccess;
    end;

def go = \rowCount. \rowWidth.
    overallSuccess <- makeRows rowWidth rowCount;

    say $ "Overall success: " ++ format overallSuccess;
    end;

go 10 20;