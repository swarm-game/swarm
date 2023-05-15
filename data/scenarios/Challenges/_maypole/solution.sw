def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def singleLeg =
    doN 4 move;
    turn left;
    end;

def singleLoop =
    doN 4 singleLeg;
    end;

doN 3 singleLoop;

doN 2 move;
turn left;
doN 2 move;
grab;