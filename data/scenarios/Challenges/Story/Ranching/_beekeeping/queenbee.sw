import "~swarm/lib/control"
import "~swarm/lib/arith"
import "~swarm/lib/tuple"
import "~swarm/lib/list"

// Spawns worker bees when structures are detected

def randomDir =
    r <- random 4;
    pure $ if (r == 1) {north}
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
        match delta \x. \y.

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

def goToHive = λmatch \hivex. \hivey.
    let depositLoc = (hivex - 1, hivey) in
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
            pure ();
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

def workerProgram = \structureLoc.
    try {make "honeycomb";} {};
    hasHoneycomb <- has "honeycomb";
    if hasHoneycomb {
        goToHive structureLoc;
    } {
        takeStepTowardItem "wildflower";
        pure ();
    };
    workerProgram structureLoc;
    end;

def mkBeeName = \structureLoc.
    "bee" ++ format structureLoc;
    end;

def workerProgramInit = \beename. \structureLoc.
    setname beename;
    appear "B" (inl ());
    workerProgram structureLoc;
    end;

def createWorkerForStructure = \loc.
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

    teleport self $ loc;
    let beename = mkBeeName loc in
    build {
        stock 1 "wax gland";
        workerProgramInit beename loc;
    };
    pure ();
    end;

def associateHive = \loc.
   let beename = mkBeeName loc in
   try {
       // Fails if the robot does not exist
       robotnamed beename;
       pure ();
   } {
       createWorkerForStructure loc;

       // Give the child robot time to register its new
       // name so that we don't end up spawning multiple
       // bees for the same location
       wait 1;
   };
   end;

/**
Each tick, iterates through all hives,
and makes sure a "bee" robot is associated with
their location.
If a structure exists without such an association,
creates a bee named after the location.
*/
def observeHives =

    beehives <- structures "beehive";
    mapM_ associateHive beehives;

    // Wait at least 1 tick so that we do not spin infinitely until
    // we saturate our computation quota for the tick.
    wait 1;
    observeHives;
    end;

def go =
    instant {observeHives};
    end;
