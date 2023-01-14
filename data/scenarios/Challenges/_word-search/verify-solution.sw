/**
Algorithm:
We only need to check the base's
current position: if we find three contiguous highlights,
then we know that the player has just completed their
third highlight.
*/

def whichOrdinal =
    isC <- ishere "lowercase c";
    if (isC) {
        return 0;
    } {
        isO <- ishere "lowercase o";
        if (isO) {
            return 1;
        } {
            isW <- ishere "lowercase w";
            if (isW) {
                return 2;
            } {
                return (-1);
            }
        }
    }
    end;
    

def countConsecutive = \expectedOrdinal. \n.

    thisOrdinal <- whichOrdinal;

    nextOrdinal <- if (thisOrdinal == expectedOrdinal) {
        return $ expectedOrdinal + 1;
    } {
        return 0;
    };

    if (nextOrdinal == 3) {
        return true;
    } {

        if (n > 1) {
            move;
            countConsecutive nextOrdinal (n - 1);
        } {
            return false;
        };
    };

    end;
    

def checkBackAndForth =

    foundBackward <- countConsecutive 0 3;
    if (foundBackward) {
        return true;
    } {
        turn back;
        countConsecutive 0 3;
    }
    end;


def checkDirections = \n.
    if (n > 0) {
        wasFound <- checkBackAndForth;
        if (wasFound) {
            return true;
        } {
            turn left;
            checkDirections $ n - 1;
        }
    } {
        return false;
    }
    end;

def checkSoln =
    checkDirections 4;
    end;

as base {checkSoln};