import "~swarm/lib/control"

def moveWithMorbidity =
    moldHere <- ishere "mold";
    if moldHere {
        try {
            // handle race conditions in which
            // another robot grabs it first
            m <- harvest;
            let spores = "mold spores" in
            if (m == spores) {
                say $ "Yuck, " ++ spores ++ "! I'm outta here.";
                selfdestruct;
            } {};
        } {};
    } {};
    move;
    end;

def goFoodDir = \f. λmatch \d. \_. 
    if (d == down) {
        foodHere <- ishere "oats";
        if foodHere {
            grab; pure ()
        } {};
        f;
        pure ()
    } {
        turn d;

        // An obstruction might arise after
        // navigation direction is determined
        // but before we move.
        try {
            moveWithMorbidity;
        } {};
        f;
    }
    end;

def goHomeDir = \f. λmatch \d. \_.
    if (d == down) {
        pure ()
    } {
        turn d;

        // An obstruction might arise after
        // navigation direction is determined
        // but before we move.
        try {
            moveWithMorbidity;
        } {};
        f;
    }
    end;

def findGoodDirection =
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
        moveWithMorbidity;
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
    nextDir <- path (inL ()) (inL homeLoc);
    case nextDir pure $ goHomeDir $ returnHome homeLoc;
    end;

def pursueFood = \hadSensedFood. \homeLoc.
    nextDir <- path (inR 5) (inR "oats");
    case nextDir (\_. if hadSensedFood {returnHome homeLoc} {pure ()}) $
        goFoodDir $ pursueFood true homeLoc;
    end;

def doMovement = \startLoc.
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

def rat =
  startLoc <- whereami;
  go startLoc;
end
