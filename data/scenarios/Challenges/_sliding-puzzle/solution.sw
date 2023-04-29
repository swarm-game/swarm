def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def getOrdinal : text -> cmd int = \item.
    count $ item ++ "-ordinal";
    end;

def go =
    doN 5 move;
    turn right;
    doN 3 move;
    turn left;

    maybeItem <- scan forward;
    // TODO Use this to solve the puzzle
    ordNum <- case maybeItem (\_. return 0) getOrdinal;
    log $ "Ordnum: " ++ format ordNum;

    drill forward;
    end;

wait 10;
//go;