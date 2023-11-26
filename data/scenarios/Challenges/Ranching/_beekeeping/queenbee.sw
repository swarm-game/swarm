// Spawns worker bees when structures are detected

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def mod : int -> int -> int = \a. \b. a - (a/b)*b end;
def abs = \n. if (n < 0) {-n} {n} end;
def min = \x. \y. if (x < y) {x} {y} end;

def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def sumTuples = \t1. \t2.
    (fst t1 + fst t2, snd t1 + snd t2);
    end;

def mapTuple = \f. \t.
    (f $ fst t, f $ snd t)
    end;

def negateTuple = \t.
    mapTuple (\x. -x) t;
    end;

def subtractTuple = \t1. \t2.
    sumTuples t1 $ negateTuple t2;
    end;

// Deprecated
def moveTuple = \tup.
    let x = fst tup in
    let y = snd tup in
    turn $ if (x > 0) {east} {west};
    doN (abs x) move;

    turn $ if (y > 0) {north} {south};
    doN (abs y) move;
    end;

def randomDir =
    r <- random 4;
    return $ if (r == 1) {north}
        $ elif (r == 2) {west}
        $ elif (r == 3) {south}
        $ else {east};
    end;

def moveHorizontal = \maxDirect. \dist.
    turn $ if (dist > 0) {east} {west};
    doN (min maxDirect $ abs dist) move;
    end;

def moveVertical = \maxDirect. \dist.
    turn $ if (dist > 0) {north} {south};
    doN (min maxDirect $ abs dist) move;
    end;

def randomStep =
    randDir <- randomDir;
    turn randDir;
    move;
    end;

def moveToward = \maxDirect. \goal.

    currLocOrig <- whereami;
    if (currLocOrig == goal) {} {

        // Include some random motion
        randomStep;

        currLoc <- whereami;
        let delta = subtractTuple goal currLoc in
        let x = fst delta in
        let y = snd delta in

        moveHorizontal maxDirect x;
        moveVertical maxDirect y;

        moveToward maxDirect goal;
    }
    end;

def watchForHoneycombRemoval = \dist.
    if (dist > 0) {
        move;
        honeycombHere <- ishere "honeycomb";
        if honeycombHere {
            watch down;
        } {};

        watchForHoneycombRemoval $ dist - 1;
    } {};
    end;

/**
Tries to find an open cell to deposit
the honeycomb. Gives up when distance
threshold exceeded.
*/
def depositHoneycomb = \dist.
    if (dist < 5) {
        emptyHere <- isempty;
        if emptyHere {
            place "honeycomb";
        } {
            move;
            depositHoneycomb $ dist + 1;
        };
    } {
        turn back;
        watchForHoneycombRemoval dist;

        // Hibernate
        wait 2000;

        // Alternative method to get rid of honeycomb
        make "buzz";
    };
    end;

def goToHive = \hiveLoc.
    let depositLoc = (fst hiveLoc - 1, snd hiveLoc) in
    moveToward 2 depositLoc;
    turn north;
    depositHoneycomb 0;
    end;

/**
Harvests an item when reached
*/
def takeStepTowardItem = \item.
    // NOTE: Max radius is hard-coded to 256
    // (see maxSniffRange in Syntax.hs)
    direction <- chirp item;
    if (direction == down) {
        // Need a try block in case
        // another bee gets here first
        try {
            harvest;
            return ();
        } {};
    } {
        // Include some random motion
        r <- random 4;
        if (r == 0) {
            randomStep;
        } {
            turn direction;
            move;
        };

        takeStepTowardItem item;
    }
    end;

/**
Searches through the existing instances of
a given structure template, starting at a supplied
index.
Either returns the (potentially new) index of the structure
(in the case that more had been built since the last check),
or unit. Re-using the newly found index amortizes the "search"
within the structure list over many ticks to constant time
rather than linear time.
*/
def findStructureNewIndex = \remainingCount. \structureLoc. \lastIdx.
    if (remainingCount > 0) {
        foundStructure <- structure "beehive" lastIdx;
        case foundStructure (\_. return $ inL ()) (\fs.
            if (structureLoc == snd fs) {
                return $ inR lastIdx;
            } {
                findStructureNewIndex (remainingCount - 1) structureLoc $ lastIdx + 1;
            }
        );
    } {
        return $ inL ();
    }
    end;

def workerProgram = \hiveIdx. \structureLoc.
    eitherFoundStructure <- structure "beehive" hiveIdx;
    case eitherFoundStructure return (\fs.
        let hasSameStructure = structureLoc == snd fs in
        if hasSameStructure {
            try {make "honeycomb";} {};
            hasHoneycomb <- has "honeycomb";
            if hasHoneycomb {
                goToHive structureLoc;
            } {
                takeStepTowardItem "wildflower";
                return ();
            };
            workerProgram hiveIdx structureLoc;
        } {
            eitherNewIdx <- findStructureNewIndex (fst fs) structureLoc hiveIdx;
            case eitherNewIdx
                (\_. selfdestruct)
                (\newIdx. workerProgram newIdx structureLoc);
        }
    );
    end;

def mkBeeName = \structureLoc.
    "bee" ++ format structureLoc;
    end;

def workerProgramInit = \beename. \hiveIdx. \structureLoc.
    setname beename;
    appear "B";
    workerProgram hiveIdx structureLoc;
    end;

def createWorkerForStructure = \structureIdx. \fs.
    // Build worker bee, assign ID, location
    create "wax gland";
    create "proboscis";

    create "ADT calculator";
    create "beaglepuss";
    create "bitcoin";
    create "branch predictor";
    create "comparator";
    create "compass";
    create "detonator";
    create "dictionary";
    create "fast grabber";
    create "GPS receiver";
    create "harvester";
    create "hourglass";
    create "lambda";
    create "net";
    create "rolex";
    create "scanner";
    create "strange loop";
    create "solar panel";
    create "treads";
    create "workbench";

    teleport self $ snd fs;
    let beename = mkBeeName (snd fs) in
    build {
        require 1 "wax gland";
        workerProgramInit beename structureIdx $ snd fs;
    };
    return ();
    end;

def associateAllHives = \remainingCount. \idx.
    if (remainingCount > 0) {

        foundStructure <- structure "beehive" idx;
        case foundStructure return (\fs.
            let beename = mkBeeName (snd fs) in
            try {
                // Fails if the robot does not exist
                robotnamed beename;
                return ();
            } {
                createWorkerForStructure idx fs;

                // Give the child robot time to register its new
                // name so that we don't end up spawning multiple
                // bees for the same location
                wait 1;
            };

            associateAllHives (remainingCount - 1) (idx + 1);
        );
    } {}
    end;

/**
Each tick, iterates through all hives,
and makes sure a "bee" robot is associated with
their location.
If a structure exists without such an association,
creates a bee named after the location.
*/
def observeHives =

    // This invocation is just to get the total structure count.
    // We will invoke it again once per iteration of 'associateAllHives'.
    foundStructure <- structure "beehive" 0;
    case foundStructure return (\fs.
        associateAllHives (fst fs) 0;
    );

    // Wait at least 1 tick so that we do not spin infinitely until
    // we saturate our computation quota for the tick.
    wait 1;
    observeHives;
    end;

def go =
    instant $ observeHives;
    end;

go;
