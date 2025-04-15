def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def abs = \n. if (n < 0) {-n} {n} end;

def λcase = \f. \g. \s. case s f g end
def λmatch = \f. \p. match p f end

def mapTuple = \f. λmatch \a. \b. (f a, f b) end;

def sumTuples = λmatch \t11. \t12. λmatch \t21. \t22.
    (t11 + t21, t12 + t22);
    end;

def negateTuple = \t.
    mapTuple (\x. -x) t;
    end;

def subtractTuple = \t1. \t2.
    sumTuples t1 $ negateTuple t2;
    end;

def moveTuple = λmatch \x. \y.
    turn $ if (x > 0) {east} {west};
    doN (abs x) move;

    turn $ if (y > 0) {north} {south};
    doN (abs y) move;
    end;

def nextRow = \d.
    intersperse 2 move $ turn d;
    end;

def harvestTrees =
    turn back;
    doN 14 move;
    turn left;

    intersperse 4 (nextRow left;) (
        intersperse 15 move harvest;
        nextRow right;
        intersperse 15 move harvest;
    );
    end;

def slatRow =
    let item = "honey frame" in
    intersperse 3 move $ place item;
    end;

def buildHive =
    // Make 16 boards
    doN 4 (make "log"; make "board");

    // Make 9 honey frames
    doN 3 $ make "log";
    doN 3 (make "board"; make "honey frame");

    doN 4 (intersperse 4 move (place "board"); turn right; move;);
    turn right;
    move;

    slatRow;
    nextRow left;
    slatRow;
    nextRow right;
    slatRow;
    end;

def moveToNextHive =
    doN 7 move;
    end;

/* Makes 8 staves */
def makeStaveBatch =
    // 1 per tree
    make "log";

    // 4 per log
    make "board";

    // 2 per board
    doN 4 $ make "stave";
    end;

def buildCasks = \caskCount.
    // 40 staves
    doN 5 makeStaveBatch;
    doN 4 $ make "steel hoop";
    doN caskCount $ make "cask";
    end;

/*
Moves forward until finding objective
item, stops when a gap is reached.
*/
def collectContiguous = \maxdist. \item. \hadFound. \dist.
    if (dist <= maxdist) {
        honeycombHere <- ishere item;
        if honeycombHere {
            grab;
            move;
            collectContiguous maxdist item true $ dist + 1;
        } {
            if hadFound {
                pure dist;
            } {
                move;
                collectContiguous maxdist item false $ dist + 1;
            }
        }
    } {
        pure dist;
    }
    end;

def collectHoneycomb =
    distTravelled <- collectContiguous 10 "honeycomb" false 0;
    turn back;
    doN distTravelled move;
    end;

def collectAllHoneycombs = \targetCount.

    honeycombHere <- ishere "honeycomb";
    if honeycombHere {} {
        watch down;
        wait 2000;
    };

    intersperse 4 (turn left; doN 9 move; turn left;) collectHoneycomb;

    currentCount <- count "honeycomb";
    if (currentCount < targetCount) {
        turn right;
        doN 27 move;
        turn right;

        collectAllHoneycombs targetCount;
    } {
        pure currentCount;
    };
    end;

def moveUntilBlocked =
    thing <- scan forward;
    let isBlocked = case thing (\_. false) (\x. x == "lakewater") in
    if isBlocked {} {
        move;
        moveUntilBlocked;
    }
    end;

def getLakewater = \caskCount.
    turn right;
    doN 2 move;
    turn left;
    doN 5 move;
    turn right;
    moveUntilBlocked;

    doN caskCount $ use "siphon" forward;
    end;

def pickRock =
    isRock <- ishere "rock";
    if isRock {
        grab;
        pure ();
    } {};
    end;

def collectRocks =
    doN 22 move;
    turn right;
    doN 59 move;
    turn left;

    doN 7 (
        intersperse 15 move pickRock;
        nextRow right;
        intersperse 15 move pickRock;
        nextRow left;
    );
    end;

def makeTables =

    doN 18 (make "log"; make "board";);
    doN 36 $ make "table";

    turn right;
    move;
    turn right;
    doN 2 move;
    doN 9 (swap "table"; move);
    doN 8 move;
    doN 9 (swap "table"; move);
    turn left;
    doN 2 move;
    turn left;
    move;
    doN 9 (swap "table"; move);
    doN 8 move;
    doN 9 (swap "table"; move);
    end;

def buildTavern =

    // x16 per rock = 
    doN 12 $ make "stone tile";

    doN 2 (
        intersperse 30 move $ place "stone tile";
        nextRow left;
        intersperse 30 move $ place "stone tile";
        nextRow right;
    );
    intersperse 30 move $ place "stone tile";

    makeTables;

    // hearth
    nextRow right;
    doN 12 move;
    intersperse 4 move (make "hearth"; swap "hearth");
    turn right;
    doN 6 move;
    turn right;
    intersperse 4 move (make "archway"; place "archway");
    nextRow right;
    intersperse 4 move (make "archway"; place "archway");
    move;

    // Make enough logs for 70 wall pieces
    doN 35 (make "log"; make "wall");
    
    intersperse 14 move (place "wall");
    turn left;
    doN 6 (move; place "wall");
    turn left;
    doN 31 (move; place "wall";);
    turn left;
    doN 6 (move; place "wall");
    turn left;
    doN 13 (move; place "wall");
    end;

def combCollectionLoop = \targetCount.

    currentCount <- count "honeycomb";
    if (currentCount < targetCount) {
        watch down;
        wait 2000;
        collectHoneycomb;
        turn back;
        combCollectionLoop targetCount;
    } {}
    end;

def buildRobot = \targetCount. \meetingLoc.

    // Unpack the "botkit"
    make "solar panel";

    build {
        require 8 "tree";
        buildHive;

        // Move to northwest corner
        turn back;
        doN 3 move;
        turn left;
        doN 4 move;
        turn left;

        combCollectionLoop targetCount;

        currLoc <- whereami;
        let delta = subtractTuple meetingLoc currLoc in
        moveTuple delta;

        // Assume that the base has already arrived
        // at the rendezvous point
        honeycombCount <- count "honeycomb";
        doN honeycombCount $ give parent "honeycomb";
    };
    end;

def placeHives = \targetCount. \meetingLoc.
    buildRobot targetCount meetingLoc;
    doN 38 move;
    buildRobot targetCount meetingLoc;
    doN 56 move;
    turn right;
    doN 15 move;
    turn left;
    buildRobot targetCount meetingLoc;
    doN 2 move;
    turn right;
    doN 26 move;
    turn left;
    buildRobot targetCount meetingLoc;
    end;

def acceptHoneyDeliveries =
    currentCount <- count "honeycomb";
    if (currentCount < 60) {
        wait 32;
        acceptHoneyDeliveries;
    } {
        // Use the "honey extractor"
        doN currentCount $ make "honey";
    }
    end;

def go =
    harvestTrees;
    turn east;
    doN 19 move;
    
    let meetingLoc = (0, -12) in
    placeHives 15 meetingLoc;

    turn right;

    buildCasks 2;
    getLakewater 2;

    turn right;

    collectRocks;

    doN 7 move;
    turn left;
    doN 80 move;
    buildTavern;

    nextRow right;
    
    currLoc <- whereami;
    let delta = subtractTuple meetingLoc currLoc in
    moveTuple delta;

    acceptHoneyDeliveries;
    doN 4 salvage;

    doN 2 $ make "mead";

    end;

go;
