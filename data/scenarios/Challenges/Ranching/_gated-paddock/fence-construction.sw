def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def processTree =
    grab;
    fenceCount <- count "fence";
    if (fenceCount < 90) {
        make "log";
        make "board";
        doN 2 $ make "fence";
    } {};
    end;

def makeHarvester =
    doN 3 $ make "log";
    doN 3 $ make "board";
    make "box";
    doN 2 $ make "wooden gear";
    make "harvester";
    install self "harvester";
    end;

def grabTwoRows =
    doN 7 (processTree; move);
    processTree;
    turn right;
    move;
    turn right;

    doN 7 (processTree; move);
    processTree;
    end;


def grabTrees =
    doN 7 move;
    turn right;

    grabTwoRows;

    turn left;
    move;
    turn left;

    grabTwoRows;

    turn left;
    move;
    turn left;

    grabTwoRows;

    end;


def harvestIfClover =
    x <- scan down;
    case x return (\y.
        if (y == "clover") {
            harvest;
            return ();
        } {};
    );
    end;


def buildFence =
    doN 6 move;
    turn right;
    doN 4 (place "fence"; move);
    turn left;
    doN 30 (place "fence"; move);
    turn left;
    doN 15 (place "fence"; move);
    turn left;
    doN 30 (place "fence"; move);
    turn left;
    doN 10 (place "fence"; move);
    make "gate";
    place "gate";
    turn right;
    end;

def gatherClover =
    doN 8 move;
    turn left;
    doN 8 move;
    turn left;

    doN 19 (harvestIfClover; move;);
    turn right;
    move;
    turn right;
    doN 19 (harvestIfClover; move;);
    turn right;
    end;

def plantCloverColumn = \direction. \extraStep.
    doN 6 (
        extraStep;
        harvest;
        move;
    );

    turn direction;
    move;
    turn direction;
    move;
    end;

def plantCloverField =
    doN 4 move;
    
    plantCloverColumn right $place "clover";
    plantCloverColumn left $ place "clover";
    plantCloverColumn right $ place "clover";
    plantCloverColumn left $ place "clover";
    plantCloverColumn right $ place "clover";
    end;


def harvestCloverField =
    doN 5 move;
    turn right;
    doN 5 move;
    turn right;
    wait 200;
    
    plantCloverColumn right $ return ();
    plantCloverColumn left $ return ();
    plantCloverColumn right $ return ();
    plantCloverColumn left $ return ();
    plantCloverColumn right $ return ();
    end;


def travelRow = \action.
    doN 28 (action; move);
    action;
    end;

def placeCloverRow =
    travelRow $ place "clover";
    end;


def distributeCloverInPaddock =
    turn left;
    doN 4 move;
    placeCloverRow;
    turn left;
    doN 2 move;
    turn left;
    placeCloverRow;
    turn right;
    doN 2 move;
    turn right;
    placeCloverRow;
    turn left;
    doN 2 move;
    turn left;
    placeCloverRow;
    end;


def pickupWool =
    x <- scan down;
    case x return (\y.
        if (y == "wool") {
            grab;
            return ();
        } {};
    );
    end;

def sweep2rows =
    travelRow pickupWool;
    turn right;
    move;
    turn right;
    travelRow pickupWool;
    turn left;
    move;
    turn left;
    end;


def sweepAreaForWool =
    doN 5 sweep2rows;
    turn left;
    doN 10 move;
    turn right;

    woolCount <- count "wool";
    if (woolCount >= 3) {
        make "sweater";
    } {};

    end;

def collectWool =
    wait 500;

    turn back;

    let forever : cmd unit -> cmd unit = \c. c ; forever c in
    forever sweepAreaForWool;
    end;


grabTrees;
makeHarvester;
buildFence;
gatherClover;
plantCloverField;
doN 3 harvestCloverField;
distributeCloverInPaddock;
collectWool;