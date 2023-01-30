/**
Generates several sequences of brackets, with some being invalid.
Remembers which rows that it generated are invalid, and loops back to check
the answer given by the player.
*/

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

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

def makeRow = \size.
    currentLoc <- whereami;

    shouldMismatch <- getRandomBool;
    mismatchIndex <- if shouldMismatch {
        random size;
    } {
        return (-1);
    };

    makeMatchedBrackets mismatchIndex 0 size;
    teleport self (fst currentLoc, snd currentLoc - 1);
    end;

def go = \size.
    doN 8 (makeRow size);
    end;

go 16;