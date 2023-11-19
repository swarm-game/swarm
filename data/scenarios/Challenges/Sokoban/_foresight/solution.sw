def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def moveUntilBlocked =
    isBlocked <- blocked;
    if isBlocked {} {
        move;
        moveUntilBlocked;
    }
    end;

def pushUntilBarrier =
    try {
        push;
        pushUntilBarrier;
    } {}
    end;

def goAroundCell =
    turn right;
    move;
    turn left;
    doN 2 move;
    turn left;
    end;

def repositionFromMiddle =
    turn right;
    move;
    turn left;
    move;
    turn right;
    moveUntilBlocked;
    end;

def childShared =
    goAroundCell;
    moveUntilBlocked;
    end;

def childSharedPrep =
    move;
    turn back;
    watch forward;
    wait 200;
    end;

def childSharedWait =
    childSharedPrep;
    pushUntilBarrier;
    end;

def firstChild =
    childShared;
    turn right;
    childSharedWait;

    doN 4 (turn left; moveUntilBlocked);
    repositionFromMiddle;
    turn left;
    childSharedWait;
    end;

def secondChild =
    childShared;

    doN 1 (turn left; moveUntilBlocked);
    
    turn right;
    childSharedWait;
    doN 3 (turn left; moveUntilBlocked);

    repositionFromMiddle;
    doN 1 (turn right; moveUntilBlocked);
    turn left;
    childSharedWait;
    end;

def thirdChild =
    childShared;
    
    doN 2 (turn left; moveUntilBlocked);
    turn right;
    childSharedWait;
    doN 2 (turn left; moveUntilBlocked);
    
    repositionFromMiddle;
    doN 2 (turn right; moveUntilBlocked);
    turn left;
    childSharedWait;
    end;

def fourthChild =
    childShared;
    doN 3 (turn left; moveUntilBlocked);
    turn right;
    childSharedWait;
    doN 1 (turn left; moveUntilBlocked);

    repositionFromMiddle;
    doN 3 (turn right; moveUntilBlocked);
    turn left;
    childSharedWait;
    end;

def fifthChild =
    childShared;
    doN 4 (turn left; moveUntilBlocked);
    turn right;
    childSharedPrep;
    doN 2 push;

    repositionFromMiddle;
    doN 4 (turn right; moveUntilBlocked);
    turn left;
    childSharedWait;

    turn right;
    move;
    turn left;
    move;
    turn left;
    doN 3 push;
    end;

def firstLeg =
    build {fifthChild};
    build {fourthChild};
    build {thirdChild};
    build {secondChild};
    build {firstChild};
    wait 10;
    
    push;
    turn right;
    move;
    turn left;
    move;
    turn left;
    pushUntilBarrier;

    wait 4;
    move;
    doN 5 (turn left; moveUntilBlocked);

    turn right;
    move;
    turn left;
    doN 2 move;
    turn left;
    doN 2 move;
    turn left;
    move;
    turn left;
    doN 3 push;
    end;

def getToCaveEntrance =
    moveUntilBlocked;
    turn right;
    move;
    turn left;
    move;
    turn right;
    move;
    turn left;
    doN 2 move;
    turn left;
    move;
    turn left;
    push;
    turn right;
    doN 2 move;
    turn right;
    push;
    turn right;
    push;
    turn left;
    doN 3 move;
    turn left;
    doN 3 move;

    x <- grab;
    equip x;
    doN 18 move;
    turn left;
    doN 5 move;
    turn left;
    move;
    turn right;
    doN 10 push;
    end;

def go =
    getToCaveEntrance;
    firstLeg;
    end;

go;
