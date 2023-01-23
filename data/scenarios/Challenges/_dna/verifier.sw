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

def waitUntilHas = \item.
    hasNow <- has item;
    if hasNow {} {
        wait 100;
        waitUntilHas item;
    };
    end;

def myStandby =
    teleport self (1, -4);
    entToClone <- grab;
    teleport self (3, -11);
    waitWhileHere "switch (off)";
    teleport self (36, -11);
    turn back;

    return entToClone;
    end;

def waitForItem : Dir -> Cmd Text = \d.
    item <- scan d;
    case item (\_. waitForItem d) return;
    end;

def placeComplements = \d. \n.
    if (n > 0) {
        item <- waitForItem d;
        let baseNumber = getNumberForBase item in
        let complementNumber = getComplementNumber baseNumber in
        let newItem = getBaseForNumber complementNumber in
        place newItem;
        move;
        placeComplements d $ n - 1;
    } {
        // selfdestruct;
    };
    end;

def placeBase = \standbyFunc. \n. 

    if (n > 0) {

        idx <- random 4;
        let entTemp = getBaseForNumber idx in
        let ent = entTemp in
        place ent;
        move;

        clonedOrganism <- placeBase standbyFunc $ n - 1;

        // Verifies the original placement order
        placedEnt <- scan down;
        isGood <- case placedEnt (\_. return false) (\x. return $ x == ent);

        move;        
        if isGood {
            return clonedOrganism;
        } {
            return "organic sludge";
        };
    } {
        // Returns the clonedOrganism
        standbyFunc;
    };
    end;

def spawnComplementer =
    buddy <- build {

        require 64 "guanine";
        require 64 "cytosine";
        require 64 "adenine";
        require 64 "thymine";

        appear "U" (inL ());

        turn right;
        move;
        turn left;
        placeComplements left 32;

        turn right;
        doN 9 move;
        turn right;
        move;

        waitUntilHas "gold coin";
        placeComplements right 32;

        appear "x" (inL ());
    };
    return buddy;
    end;

def makeDnaStrand =
    teleport self (5, -2);
    r <- spawnComplementer;
    clonedOrganism <- placeBase myStandby 32;
    doN 3 move;
    drill forward;
    teleport self (36, -12);
    give r "gold coin";

    return clonedOrganism
    end;

def go =
    instant $ waitUntilHere "flower";
    // _f <- grab;
    clonedOrganism <- makeDnaStrand;
    teleport self (40, -13);
    place clonedOrganism;
    end;

go;