def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def getBaseForNumber = \n.
    if (n == 0) {
        "guanine";
    } {
        if (n == 1) {
            "cytosine";
        } {
            if (n == 2) {
                "adenine";
            } {
                "thymine";
            };
        };
    };
    end;

def getNumberForBase = \n.
    if (n == "guanine") {
        0;
    } {
        if (n == "cytosine") {
            1;
        } {
            if (n == "adenine") {
                2;
            } {
                if (n == "thymine") {
                    3;
                } {-1};
            };
        };
    };
    end;

def getComplementNumber = \n.
    if (n == 0) {
        1;
    } {
        if (n == 1) {
            0;
        } {
            if (n == 2) {
                3;
            } {
                2;
            };
        };
    };
    end;

def waitUntilHere = \remainingCount.
    if (remainingCount > 0) {
        maybeItemHere <- scan down;
        case maybeItemHere (\_. 
            watch down;
            wait 1000;
            waitUntilHere remainingCount;
        ) (\itemHere.

            maybeItemAbove <- scan left;
            case maybeItemAbove (\_. fail "Expected an item here.") (\itemAbove.
                let num = getNumberForBase itemAbove in
                if (num >= 0) {
                    let complementNum = getComplementNumber num in
                    let complementItem = getBaseForNumber complementNum in
                    if (complementItem == itemHere) {
                        move;
                        waitUntilHere $ remainingCount - 1;
                    } {
                        create "pixel (R)";
                    }
                } {
                    fail "Expected nonnegative item index."
                }
            );
        );
    } {
        log "Finished verifying top row";
        create "pixel (G)";
    };

    end;

def go =
    instant $ waitUntilHere 32;
    end;

go;
