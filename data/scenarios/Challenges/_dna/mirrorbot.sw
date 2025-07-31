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

/** Toggle the lowest bit */
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

def placeComplementOf = \item.
    let baseNumber = getNumberForBase item in
    if (baseNumber >= 0) {
        let complementNumber = getComplementNumber baseNumber in
        let newItem = getBaseForNumber complementNumber in
        move;
        create newItem;
        place newItem;
    } {
        let sludge = "organic sludge" in
        create sludge;
        place sludge;
    }
    end;

def waitUntilHere =
    watch down;
    wait 1000;

    maybeItemDown <- scan down;
    case maybeItemDown (\_. waitUntilHere) (\itemHere.
        placeComplementOf itemHere;
    );
    end;

def waitUntilEmpty =
    watch down;
    wait 1000;
    emptyHere <- isempty;
    if emptyHere {
        // reset the position
        turn back;
        move;
        turn back;
    } {
        waitUntilEmpty;
    }
    end;

def go =
    instant {waitUntilHere};
    waitUntilEmpty;
    go;
    end;

go;
