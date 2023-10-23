
def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def nextRow = \d.
    turn d;
    move;
    turn d;
    end;

def slatRow =
    make "slat";
    intersperse 3 move (place "slat");
    end;

def buildHive =
    doN 4 (intersperse 4 move (place "board"); turn right; move;);
    turn right;
    move;

    slatRow;
    nextRow left;
    slatRow;
    nextRow right;
    slatRow;

    end;

def moveToNextHive =
    turn left;
    doN 7 move;
    turn left;
    doN 3 move;
    turn right;
    end;

def buildTankSide = \item.
    doN 3 (move; place item;);
    move;
    turn right;
    move;
    place item;
    turn left;
    move;
    turn right;
    end;

def buildBrewery =
    turn right;
    doN 3 (place "copper pipe"; move;);
    move;
    turn right;
    doN 3 move;
    turn right;
    move;
    doN 4 $ buildTankSide "copper wall";
    end;

def go =
    intersperse 4 moveToNextHive buildHive;

    doN 10 move;
    buildBrewery;
    end;

go;