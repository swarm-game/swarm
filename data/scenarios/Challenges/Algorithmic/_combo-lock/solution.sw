import "~swarm/lib/scan"

def moveToLock =
    emptyHere <- isempty;
    if emptyHere {move; moveToLock} {};
    end;

def cycleCombos = \n.
    entityNorth <- scan north;
    if (entityNorth == "gate") {
        if (n > 0) {
            drill down;
            nextEnt <- scan east;
            if (nextEnt == "") {} {turn east; move; cycleCombos 3};
            cycleCombos $ n - 1;
        } {
            turn west;
            move;
        };
    } {}
    end;

def moveUntilBlocked =
    isblocked <- blocked;
    if isblocked {} {
        move;
        moveUntilBlocked;
    };
    end;

def toLeftEdge =
    turn north;
    move;
    turn left;
    moveUntilBlocked;
    turn north;
    end;

def goUp =
    toLeftEdge;
    move; move; move;
    end;

def grabBitcoin =
    move; move;
    turn right;
    move; move;
    grab;
    end;

def go =
    moveToLock;
    cycleCombos 3;
    goUp;
    cycleCombos 3;
    goUp;
    cycleCombos 3;
    goUp;
    cycleCombos 3;

    toLeftEdge;
    grabBitcoin;
    end;
