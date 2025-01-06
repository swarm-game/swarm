def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def placeBoulder =  \border.
    create border;

    isEmptyHere <- isempty;
    if isEmptyHere {
        place border
    } {
        swap border;
        pure ();
    };
    
    pure ();
end

def spinWeb = \border.
    move;
    turn left;

    try {
        intersperse 2 move $ placeBoulder border;
        doN 3 (
            turn left;
            doN 2 (placeBoulder border; move);
        );
        placeBoulder border;
    } {};
end

def goDir = \result.
    let d = fst result in
    if (d == down) {
        spinWeb "mountain";
    } {
        turn d; move;
    };
end;

def followRoute = \item.
    nextDir <- path (inL ()) (inR item);
    case nextDir pure goDir;
    followRoute item;
    end;

myname <- whoami;
let target_object = if (myname == "tree spider") {
    "tree";
} {
    "flower"
} in

followRoute target_object;