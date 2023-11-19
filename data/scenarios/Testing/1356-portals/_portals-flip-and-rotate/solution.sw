def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def abs = \n. if (n < 0) {-n} {n} end;

def mapTuple = \f. \t.
    (f $ fst t, f $ snd t)
    end;

def sumTuples = \t1. \t2.
    (fst t1 + fst t2, snd t1 + snd t2);
    end;

def negateTuple = \t.
    mapTuple (\x. -x) t;
    end;

def getRelativeLocation = \absCurrentLoc. \absDestLoc.
    let negatedLoc = negateTuple absCurrentLoc in
    return $ sumTuples negatedLoc absDestLoc;
    end;

def moveTuple = \tup.
    let x = fst tup in
    let y = snd tup in
    turn $ if (x > 0) {east} {west};
    doN (abs x) move;
    turn $ if (y > 0) {north} {south};
    doN (abs y) move;
    end;

def goToLocation = \currentLoc. \absoluteDestination.
    relativeDestination <- getRelativeLocation currentLoc absoluteDestination;
    moveTuple relativeDestination;
    end;

def goToBottom =
    turn south; doN 14 move;
    end;

def go =
    goToLocation (0, 0) (3, -2);
    goToLocation (0, 0) (12, -2);
    goToLocation (0, 0) (18, -5);
    goToLocation (0, 0) (23, -3);

    goToBottom;
    goToLocation (0, -14) (3, -12);
    goToBottom;
    goToLocation (0, -14) (9, -9);
    goToBottom;
    goToLocation (0, -14) (18, -9);
    goToBottom;
    goToLocation (0, -14) (26, -10);

    turn east;
    doN 29 move;
    goToBottom;
    grab;
    end;

go;
