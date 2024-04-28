/**
Swims back and forth forever.
*/

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def swim =
    appear "^" (inl ());
    doN 3 (move; wait 3);
    turn back;
    wait 15;
    doN 3 (move; wait 3);
    turn back;
    appear " " (inl ());
    end;

def go =
    waitR <- random 100;
    wait $ 50 + waitR;
    swim;
    go;
    end;

go;
