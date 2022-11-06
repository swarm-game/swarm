def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;


def processTree =
    grab;
    make "log";
    make "board";
    doN 2 (make "fence");
    end;


def grabTwoRows =
    doN 7 (processTree; move);
    processTree;
    turn right;
    move;
    turn right;

    doN 7 (processTree; move);
    processTree;
    end;


def grabTrees =
    doN 7 move;
    turn right;

    grabTwoRows;

    turn left;
    move;
    turn left;

    grabTwoRows;

    turn left;
    move;
    turn left;

    grabTwoRows;

    end;


def buildFence =
    doN 6 move;
    turn right;
    doN 4 (place "fence"; move);
    turn left;
    doN 30 (place "fence"; move);
    turn left;
    doN 15 (place "fence"; move);
    turn left;
    doN 30 (place "fence"; move);
    turn left;
    doN 7 (place "fence"; move);
    place "fence";
    turn right;
    doN 6 move;
    end;

// buildFence;
grabTrees;
buildFence;