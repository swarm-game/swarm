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

def waitUntilOccupied =
    stillEmpty <- isempty;
    if stillEmpty {
        watch down;
        wait 1000;
        waitUntilOccupied;
    } {};
    end;

def myStandby = \receptacleLoc.
    teleport self receptacleLoc;
    entToClone <- grab;
    teleport self (36, -11);
    turn back;
    return $ inR entToClone;
    end;

def placeBase = \standbyFunc. \n. 

    if (n > 0) {
        idx <- random 4;
        let entTemp = getBaseForNumber idx in
        let ent = entTemp in
        create ent;
        place ent;
        move;

        clonedOrganism <- placeBase standbyFunc $ n - 1;

        // Unwinds the stack; verifies the original placement order
        placedEnt <- instant waitUntilSomethingExists;
        let isGood = ent == placedEnt in
        move;

        if isGood {
            return clonedOrganism;
        } {
            return $ inL ();
        }
    } {
        // Returns the clonedOrganism
        standbyFunc;
    };
    end;

def makeDnaStrand = \receptacleLoc.
    teleport self (5, -2);

    dims <- floorplan "DNA decoder";
    let decoderWidth = fst dims in
    eitherClonedOrganism <- placeBase (myStandby receptacleLoc) decoderWidth;

    case eitherClonedOrganism (\_.
        create "pixel (R)";
    ) (\clonedItem.
        instant $ (
            teleport self (0, -11);
            waitUntilHere "switch (on)";

            bottomWaypointQuery <- waypoint "receiver" 1;
            let receptacleLoc2 = snd bottomWaypointQuery in
            teleport self receptacleLoc2;

            sow clonedItem;
            create clonedItem;
            k <- robotnamed "keeper";
            give k clonedItem;

            let slideBox = "slide box" in
            create slideBox;
            give base slideBox;
            say $ "You got a new \"" ++ slideBox ++ "\"";
        );
    );
    end;

def waitForCloneableOrganism =

    waypointQuery <- waypoint "receiver" 0;
    let receptacleLoc = snd waypointQuery in
    organism <- instant (
        teleport self receptacleLoc;

        waitUntilOccupied;

        thingHere <- scan down;
        case thingHere (\x. return $ inL x) (\item.
            isOrganism <- hastag item "organism";
            return $ inR item;
        );
    );

    case organism (\_.
        say "Not a cloneable organism!";
        waitForCloneableOrganism;
    ) (\item.
        create "pixel (G)";
        makeDnaStrand receptacleLoc;
    );
    end;

def go =
    waitForCloneableOrganism;
    turn east;
    go;
    end;

go;
