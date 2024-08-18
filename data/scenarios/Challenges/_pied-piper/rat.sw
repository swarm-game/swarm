def checkExpireSelf =
    loc <- whereami;
    if (snd loc > 6) {selfdestruct} {};
    end;


def moveWithMortality =
    move;
    instant checkExpireSelf;
    end;

def goFoodDir = \f. \r.
    // say "in goFoodDir";
    let d = fst r in
    if (d == down) {
        foodHere <- ishere "oats";
        if foodHere {
            grab; return ()
        } {};
        f;
        return ()
    } {
        turn d;

        // An obstruction might arise after
        // navigation direction is determined
        // but before we move.
        try {
            moveWithMortality;
        } {};
        f;
    }
    end;

def goHomeDir = \f. \r.
    // say "in goHomeDir";
    let d = fst r in
    if (d == down) {
        return ()
    } {
        turn d;

        // An obstruction might arise after
        // navigation direction is determined
        // but before we move.
        try {
            moveWithMortality;
        } {};
        f;
    }
    end;

def findGoodDirection =
    // say "in findGoodDirection";
    isBlocked <- blocked;
    if isBlocked {
        turn left;
        findGoodDirection;
    } {};
    end;

def moveUntilBlocked =
    isBlocked <- blocked;
    if isBlocked {
    } {
        moveWithMortality;
        moveUntilBlocked;
    };
    end;

def pauseAtRandom =
    r <- random 3;
    if (r == 0) {
        r2 <- random 12;
        wait $ 6 + r2;
    } {}
    end;

def returnHome = \homeLoc.
    // say "in returnHome";
    nextDir <- path (inL ()) (inL homeLoc);
    case nextDir return $ goHomeDir $ returnHome homeLoc;
    end;

def pursueFood = \hadSensedFood. \homeLoc.
    // say $ "in pursueFood. hadSensedFood? " ++ format hadSensedFood;
    nextDir <- path (inR 5) (inR "oats");
    case nextDir (\_. if hadSensedFood {returnHome homeLoc} {return ()}) $
        goFoodDir $ pursueFood true homeLoc;
    end;


def doMovement = \startLoc.
    // say "in doMovement";
    checkExpireSelf;

    findGoodDirection;
    moveUntilBlocked;
    pauseAtRandom;
    pursueFood false startLoc;
    end;

/** Loop */
def go = \startLoc.
    doMovement startLoc;
    go startLoc;
    end;

startLoc <- whereami;
// let shouldDestruct = !(snd startLoc == -17 && fst startLoc < 19) in
let shouldDestruct = false in
if shouldDestruct {
    selfdestruct;
} {
    go startLoc;
}
