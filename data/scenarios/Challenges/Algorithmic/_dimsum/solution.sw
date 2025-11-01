def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def moveOne =
    move;
    move;
    turn back;
    push;
    turn back;
    end;

def oneLap =
    doN 4 (
        doN 8 moveOne;
        turn right;
    );
    end;

def moveCartToDiagonal =
    doN 16 oneLap;
    end;


def go =
    doN 4 moveCartToDiagonal;
    end;

go;
