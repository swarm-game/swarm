def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def forever = \c. c; forever c; end;

def pollUntilHas = \item.
    itemInInventory <- has item;
    if itemInInventory {} {
        pollUntilHas item;
    };
    end;

def pollUntilCount = \targetCount. \item.
    itemCount <- count item;
    if (itemCount < targetCount) {
        pollUntilCount targetCount item;
    } {};
    end;

def waitToGiveUntilHas = \item. \recipient.
    pollUntilHas item;
    give recipient item;
    end;

/** The "depth" is the number of spaces between
the root robot and the target item.
It is one minus the distance to the target item.
*/
def recurseUntilDepth = \depth.
    let item = "coal lump" in
    child <- build {

        // NOTE: Treads are auto-installed via the implicit "require"
        move;
        unequip "treads";

        pollUntilCount depth "repro kit";

        // Use the recipe from "repro kit"
        make "3D printer";

        equip "solar panel";
        equip "grabber";
        equip "3D printer";
        equip "dictionary";
        equip "branch predictor";
        equip "comparator";
        equip "calculator";
        equip "lambda";
        equip "strange loop";
        equip "string";
        equip "clock";
        equip "counter";
        equip "mirror";
        equip "scanner";
        equip "logger";
        equip "net";

        if (depth > 1) {
            recurseUntilDepth $ depth - 1;
            forever $ waitToGiveUntilHas item parent;
        } {
            make "bucketwheel excavator";
            forever (
                drill forward;
                give parent item;
            );
        };
    };

    let kitGiftCount = depth in
    // say $ "Going to give child " ++ (format kitGiftCount) ++ " repro kits.";
    doN kitGiftCount $ give child "repro kit";

    end;

def makeBriquette =
    let briquette = "coal briquette" in
    pollUntilCount 100 "coal lump";
    make briquette;
    briquetteAlreadyPlaced <- ishere briquette;
    if briquetteAlreadyPlaced {} {
        place briquette;
    };
    end;

def go =
    // Use the recipe from "repro kit" to unpack the right
    // set of items to be taken from the inventory with the implicit "require":
    make "3D printer";
    recurseUntilDepth 8;
    forever makeBriquette;
    end;

go;