def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def crossPath =
    doN 6 move;
    turn back;
    wait 5;
    end;

def go =
    crossPath;
    go;
    end;

def start =
    pos <- whereami;
    wait $ fst pos;
    go;
    end;
    
start;
