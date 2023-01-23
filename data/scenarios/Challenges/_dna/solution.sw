def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

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
    doN 5 move;
    end;

def moveToOtherRow =
    turn right;
    doN 9 move;
    turn right;
    move;
    end;

def waitForItem : Dir -> Cmd Text = \d.
    item <- scan d;
    case item (\_. waitForItem d) return;
    end;

/**
Store the observed entities in the recursion stack.
*/
def replicatePattern = \standbyFunc. \n.
    if (n > 0) {
        thingTemp <- waitForItem down;
        let thing = thingTemp in
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
    waitWhileHere sentinel;
    moveToPattern;
    replicatePattern moveToOtherRow 32;
    
    // Activate the switch
    doN 3 move;
    drill forward;
    end;

go;