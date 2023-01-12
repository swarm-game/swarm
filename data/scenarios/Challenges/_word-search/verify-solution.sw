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

def countHighlighted = \n.
    resetBits;
    countInRow move isHighlighted n;
    hCount <- count "bit (1)";
    say $ "Found " ++ format hCount;
    return hCount;
    end;

def moveToRowBeginning = \r.
    currentPos <- as r {whereami};
    teleport r (0, snd currentPos);
    end;

def checkSoln =

    r <- robotnamed "lettersetter";
    moveToRowBeginning r;
    as r {
        finishedPlacing <- has "boulder";
        if (finishedPlacing) {
            hCount <- countHighlighted 30;
            return $ hCount >= 3;
        } {
            return false;
        };
    };
    end;

checkSoln;
