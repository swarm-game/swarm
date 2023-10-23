// Spawns worker bees when structures are detected

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def mod : int -> int -> int = \a. \b. a - (a/b)*b end;
def abs = \n. if (n < 0) {-n} {n} end;

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

def moveTuple = \tup.
    let x = fst tup in
    let y = snd tup in
    turn $ if (x > 0) {east} {west};
    doN (abs x) move;

    turn $ if (y > 0) {north} {south};
    doN (abs y) move;
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
    currLoc <- whereami;
    let depositLoc = (fst hiveLoc - 1, snd hiveLoc) in
    let delta = subtractTuple depositLoc currLoc in
    moveTuple delta;
    turn north;
    depositHoneycomb 0;
    end;

/**
Harvests an item when reached
*/
def takeStepTowardItem = \item.
    direction <- chirp item;
    if (direction == down) {
        // Need a try block in case
        // another bee gets here first
        try {
            harvest;
            return ();
        } {};
    } {
        turn direction;
        move;
        takeStepTowardItem item;
    }
    end;

def workerProgram = \hiveIdx. \structureLoc.
    foundStructure <- structure "beehive" hiveIdx;
    let stillHasStructure = case foundStructure (\_. false) (\fs.
        structureLoc == snd fs;
    ) in

    if (stillHasStructure) {

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
        selfdestruct;
    }
    end;

def workerProgramInit = \hiveIdx. \structureLoc.
    appear "B";
    if (mod hiveIdx 2 == 0) {turn left;} {};
    workerProgram hiveIdx structureLoc;
    end;

def observeHives = \lastHiveCount.

    foundStructure <- structure "beehive" lastHiveCount;
    newHiveCount <- case foundStructure (\_. return lastHiveCount) (\fs. 
        let newHiveCount = fst fs in

        if (newHiveCount > lastHiveCount) {
            // Build worker bee, assign ID, location
            create "wax gland";
            create "proboscis";

            create "solar panel";
            create "treads";
            create "detonator";
            create "harvester";
            create "fast grabber";
            create "workbench";
            create "GPS receiver";
            create "scanner";
            create "rolex";
            create "beaglepuss";
            create "branch predictor";
            create "comparator";
            create "compass";
            create "ADT calculator";
            create "dictionary";
            create "lambda";
            create "strange loop";
            create "hourglass";
            create "net";

            teleport self $ snd fs;
            build {
                require 1 "wax gland";
                workerProgramInit lastHiveCount $ snd fs;
            };
            return ();
        } {};

        return newHiveCount;
    );

    wait 1;
    observeHives newHiveCount;
    end;

def go =
    instant $ observeHives 0;
    end;

go;
