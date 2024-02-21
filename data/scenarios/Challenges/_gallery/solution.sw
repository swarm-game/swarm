def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def countRow = \currentCount.
    emptyHere <- isempty;
    if emptyHere {
        return currentCount;
    } {
        move;
        countRow $ 1 + currentCount;
    };
    end;

def getOrdinal = \d.
    maybeEntity <- scan d;
    case maybeEntity (\nothing. return $ inL nothing) (\item.
        myCount <- count item;
        return $ inR myCount;
    );
    end;

/*
Precondition:
Facing west, just north of the row.
*/
def moveToBeginning =
    thingLeft <- scan left;
    case thingLeft (\_.
        turn back;
        move;        
    ) (\_.
        move;
        moveToBeginning;
    );
    end;

def doSwap =
    turn right;
    push;
    turn right;
    move;
    move;
    turn back;
    push;
    move;
    move;
    turn right;
    move;
    turn right;
    push;
    turn left;
    move;
    turn right;
    move;
    turn right;
    push;
    move;
    move;
    turn right;
    move;
    end;

/*
Precondition:
Standing just north of the eastern
member in the pair, and facing east.

Postcondition:
Moved one space east from staring position
in the same orientation.
*/
def swapAdjacent = \remainingSteps. \previousOrdinal.
    thisOrdinal <- if (remainingSteps > 1) {
        getOrdinal right;
    } {
        return $ inL ();
    };

    case thisOrdinal (\_.
        turn back;
        move;
        moveToBeginning;
    ) (\num.
        let shouldSwap = num < previousOrdinal in
        ordinal <- if shouldSwap {
            doSwap;
            return previousOrdinal;
        } {
            return num;
        };
        move;
        swapAdjacent (remainingSteps - 1) ordinal;
    );
    end;

def doLoop = \unsortedCount.
    if (unsortedCount > 1) {
        prevOrdinal <- getOrdinal right;
        move;
        case prevOrdinal return $ swapAdjacent unsortedCount;
        doLoop $ unsortedCount - 1;
    } {}
    end;

def go =
    move;
    move;
    move;
    totalCount <- countRow 0;
    turn back;
    doN totalCount move;
    turn right;
    move;
    turn right;
    doLoop totalCount;
    end;

go;
