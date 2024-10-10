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

def myStandby = \recepticleLoc.
    teleport self recepticleLoc;
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

def makeDnaStrand = \recepticleLoc.
    teleport self (5, -2);

    dims <- floorplan "DNA decoder";
    let decoderWidth = fst dims in
    eitherClonedOrganism <- placeBase (myStandby recepticleLoc) decoderWidth;

    case eitherClonedOrganism (\_.
        create "pixel (R)";
    ) (\clonedItem.
        instant $ (
            teleport self (0, -11);
            waitUntilHere "switch (on)";

            bottomWaypointQuery <- waypoint "receiver" 0;
            let recepticleLoc2 = snd bottomWaypointQuery in
            teleport self recepticleLoc2;
            place clonedItem;
        );
    );
    end;

def go =
    waypointQuery <- waypoint "receiver" 1;
    let recepticleLoc = snd waypointQuery in
    instant (
        teleport self recepticleLoc;
        // TODO Make this a "tag" category
        waitUntilHere "flower";
    );
    create "pixel (G)";
    makeDnaStrand recepticleLoc;
    end;

go;
