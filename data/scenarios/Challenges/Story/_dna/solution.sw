import "~swarm/lib/control"
import "~swarm/lib/list"
import "common"

def moveUntilBlocked =
    blockedHere <- blocked;
    if blockedHere {} {
        move;
        moveUntilBlocked;
    }
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
    doN 5 move;
    turn left;
    doN 4 move;
    turn left;
    move;
    end;

def moveToOtherRow =
    turn right;
    doN 2 move;
    turn right;
    doN 4 move;
    turn left;
    doN 4 move;
    turn left;
    doN 4 move;
    turn right;
    doN 2 move;
    turn right;
    move;
    end;

def waitForItem : Dir -> Cmd Text = \d.
    item <- scan d;
    case item (\_.
        watch d;
        wait 1000;
        waitForItem d;
    ) pure;
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

/**
Position self at entrance
*/
def pickFlowerAndWater =
    doN 6 move;
    dahlia <- grab;

    turn left;
    doN 8 move;
    turn right;
    doN 31 move;
    clover <- grab;
    turn back;
    doN 35 move;
    d <- grab;
    doN 10 move;

    turn left;

    doN 18 move;
    use "siphon" forward;
    turn left;
    doN 7 (
        move;
        use "siphon" right;
    );
    doN 4 move;
    turn right;
    doN 16 move;

    mushroom <- grab;

    turn back;
    doN 23 move;

    turn right;
    pure dahlia;

    // pure mushroom;
    // pure d;
    end;


def waitUntilOccupied =
    stillEmpty <- isempty;
    if stillEmpty {
        watch down;
        wait 1000;
        waitUntilOccupied;
    } {};
    end;

def returnToInputReceptacle =
    turn back;
    doN 5 move;
    turn left;
    moveUntilBlocked;
    turn left;
    doN 29 move;
    turn right;
    moveUntilBlocked;
    turn left;
    doN 4 move;
    turn right;
    doN 6 move;
    turn right;
    moveUntilBlocked;
    end;

def completeDnaTask = \sentinel.
    place sentinel;
    make "specimen slide";

    doN 16 $ make "cytosine";

    waitWhileHere sentinel;
    moveToPattern;
    replicatePattern moveToOtherRow 32;

    // Activate the switch
    doN 3 move;
    drill forward;
    wait 2;
    drill forward;

    turn left;
    doN 6 move;
    turn left;
    doN 32 move;
    turn left;
    doN 2 move;
    turn left;
    move;

    waitUntilOccupied;
    grab;

    returnToInputReceptacle;
    end;

def go =
    _sentinel <- pickFlowerAndWater;
    moveUntilBlocked;

    let organisms = tagmembers "organism" in
    mapM_ completeDnaTask organisms;
    end;
