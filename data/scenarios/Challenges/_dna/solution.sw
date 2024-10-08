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
                3;
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

def waitWhileHere = \item.
    stillHere <- ishere item;
    if stillHere {
        watch down;
        wait 1000;
        waitWhileHere item;
    } {};
    end;

def waitUntilHere = \item.
    hereNow <- ishere item;
    if hereNow {} {
        watch down;
        wait 1000;
        waitUntilHere item;
    };
    end;

def moveToPattern =
    turn back;
    move;
    turn right;
    doN 2 move;
    turn right;
    doN 4 move;
    turn right;
    move;
    turn left;
    move;
    end;

def moveToOtherRow =
    turn right;
    doN 8 move;
    turn right;
    move;
    end;

def waitForItem : Dir -> Cmd Text = \d.
    item <- scan d;
    case item (\_. 
        watch d;
        wait 1000;
        waitForItem d;
    ) return;
    end;

def waitForSpecificItem = \item. \d.
    itemIsHere <- ishere item;
    if itemIsHere {
    } {
        watch d;
        wait 1000;
        waitForSpecificItem item d;
    }
    end;

def placeComplementOf = \item.
    let baseNumber = getNumberForBase item in
    let complementNumber = getComplementNumber baseNumber in
    let newItem = getBaseForNumber complementNumber in
    place newItem;
    end;

/**
Store the observed entities in the recursion stack.
*/
def replicatePattern = \standbyFunc. \n.
    if (n > 0) {
        thingTemp <- waitForItem left;
        let thing = thingTemp in
        placeComplementOf thing;
        move;
        replicatePattern standbyFunc $ n - 1;

        place thing;
        move;
    } {
        standbyFunc;
    }
    end;

def go =
    move;
    let sentinel = "flower" in
    place sentinel;

    doN 16 $ make "cytosine";

    waitWhileHere sentinel;
    moveToPattern;
    replicatePattern moveToOtherRow 32;
    
    // Activate the switch
    doN 3 move;
    drill forward;
    turn left;
    doN 4 move;
    turn left;
    doN 40 move;
    turn left;
    doN 2 move;
    turn left;
    move;

    waitForSpecificItem "flower" down;
    grab;

    end;

go;
