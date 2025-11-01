def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

/**
False = Left
*/
def boolAsLeftRight = \b.
    if b {right} {left}
    end;

def squareSide =
    drill down;
    doN 5 move;
    end;

def goToFirstShop =
    doN 3 move;
    doN 5 (doN 2 move; grab);
    turn left;
    doN 2 move;
    end;

def doPartialFigureEight =
    intersperse 2 (turn left) squareSide;
    intersperse 4 (turn right) squareSide;
    end;

def doFullFigureEight = \startLeft.
    intersperse 4 (turn $ boolAsLeftRight $ not startLeft) squareSide;
    intersperse 4 (turn $ boolAsLeftRight startLeft) squareSide;
    end;

goToFirstShop;

doN 1 doPartialFigureEight;
doN 9 $ doFullFigureEight true;

doN 8 $ doFullFigureEight false;
squareSide;
drill down;