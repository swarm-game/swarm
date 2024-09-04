def doWalk =
    move;
    move;
    move;
    end;

def doTurn =
    turn left;
    end;

def runSomeFunction = \f.
    f;
    end;

def go =
    doWalk;
    wait 2;
    runSomeFunction doTurn;
    wait 2;
    go;
    end;

go;
