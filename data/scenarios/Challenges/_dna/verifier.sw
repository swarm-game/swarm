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

def waitUntilSomethingExists =
    maybeItemHere <- scan down;
    case maybeItemHere (\_. 
        watch down;
        wait 1000;
        waitUntilSomethingExists;
    ) return;
    end;

def waitUntilHere = \item.
    hereNow <- ishere item;
    if hereNow {} {
        watch down;
        wait 1000;
        waitUntilHere item;
    };
    end;

def myStandby =
    teleport self (1, -4);
    entToClone <- grab;
    teleport self (36, -11);
    turn back;
    return entToClone;
    end;

def placeBase = \standbyFunc. \n. 

    if (n > 0) {

        idx <- random 4;
        let entTemp = getBaseForNumber idx in
        let ent = entTemp in
        place ent;
        move;

        didSucceed <- placeBase standbyFunc $ n - 1;

        // Unwinds the stack; verifies the original placement order
        placedEnt <- instant waitUntilSomethingExists;
        let isGood = ent == placedEnt in
        move;
        return $ didSucceed && isGood;
    } {
        // Returns the clonedOrganism
        standbyFunc;
        return true;
    };
    end;

def makeDnaStrand =
    teleport self (5, -2);
    finalSuccess <- placeBase myStandby 32;

    if (finalSuccess) {
        teleport self (40, -13);
        place "flower";
    } {
        create "pixel (R)";
    }
    // say $ "Final success: " ++ format finalSuccess;
    end;

def go =
    instant $ waitUntilHere "flower";
    create "pixel (G)";
    makeDnaStrand;
    end;

go;
