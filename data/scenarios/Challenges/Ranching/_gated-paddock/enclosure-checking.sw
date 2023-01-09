// Algorithm:
// ----------
// Maintain current direction until a wall is encountered.
// Then enter "wall-following mode".
// This mode presumes the wall is not a loop.
// Wall-following mode exploits recursion to keep track of how many left turns were made
// and then unwinds them again by ensuring each is paired with a right turn.
// Once the recursion is fully unwound, the robot proceeds along its original direction
// (though it may now be laterally displaced).
//
// (If it was a loop, then an "oriented breadcrumb" would need to be left.
// The breadcrumb is oriented in case a single-width passage is backtracked
// along the opposite wall.)

/** A "gate" is walkable, so we need to supplement the "blocked" check with this function.
Since fences are "unwalkable", they do not need to be mentioned in this function.
*/
def isFenced =
    s <- scan forward;
    return (
        case s
            (\_. false)
            (\x. x == "gate")
    );
    end;

def isBlockedOrFenced =
    b <- blocked;
    f <- isFenced;
    return (b || f);
    end;

// Returns true if we've already placed two
// breadcrumbs on a given tile, false otherwise.
def leaveBreadcrumbs =
    
    let bc1 = "fresh breadcrumb" in
    let bc2 = "treaded breadcrumb" in

    wasTraversedOnce <- ishere bc1;
    if wasTraversedOnce {
        _crumb <- grab;
        make bc2;
        place bc2;
        return false;
    } {
        wasTraversedTwice <- ishere bc2;
        if wasTraversedTwice {
            return true;
        } {
            // Make sure nothing's in the way before we place
            // our breadcrumb:
            x <- scan down;
            case x return (\y.
                // If we're on a water tile, get rid of
                // it with our special "drilling" recipe
                if (y == "water") {
                    drill down;
                    // Nothing will remain on the ground.
                    // after making the "steam" via
                    // the drilling recipe.
                } {
                    grab;
                    return ();
                };
            );

            make bc1;
            place bc1;
            return false;
        };
    };
    end;

def goForwardToPatrol = \wasBlocked.
    b <- isBlockedOrFenced;
    if b {
        turn left;
        goForwardToPatrol true;
        turn right;
        goForwardToPatrol false;
    } {
        if wasBlocked {
            isLoop <- leaveBreadcrumbs;
            if isLoop {
                fail "loop";
            } {};
        } {};
        move;
    };
    end;

/**
There should only be one place in the
code where an exception is thrown: that is,
if a treaded breadcrumb is encountered.
*/
def checkIsEnclosedInner =
    try {
        goForwardToPatrol false;
        // Water is the outer boundary
        hasWater <- ishere "water";
        if hasWater {
            return false;
        } {
            checkIsEnclosedInner;
        };
    } {
        return true;
    };
    end;

def checkIsEnclosed =

    // The "evaporator" drill is used
    // to clear water tiles.
    let specialDrill = "evaporator" in
    create specialDrill;
    equip specialDrill;

    // NOTE: System robots can walk on water
    // so we only need this if we want to
    // demo the algorithm with a player robot.
//    create "boat";
//    equip "boat";

    checkIsEnclosedInner;
    end;

def boolToInt = \b. if (b) {return 1} {return 0}; end;

def countAdjacentBlockages =

    turn left;
    b1 <- isBlockedOrFenced;
    c1 <- boolToInt b1;

    turn left;
    b2 <- isBlockedOrFenced;
    c2 <- boolToInt b2;

    turn left;
    b3 <- isBlockedOrFenced;
    c3 <- boolToInt b3;

    turn left;
    b4 <- isBlockedOrFenced;
    c4 <- boolToInt b4;

    return $ c1 + c2 + c3 + c4;
    end;

// Step forward, observing left and right.
def observeLeftAndRight =
    move;
    turn left;
    amBlockedLeft <- isBlockedOrFenced;
    val1 <- boolToInt amBlockedLeft;

    turn back;
    amBlockedRight <- isBlockedOrFenced;
    val2 <- boolToInt amBlockedRight;

    turn right;
    move;
    return $ val1 + val2;
    end;


/** If the four cardinal directions have at most
one blockage, then there will exist an orientation
where both that direction and its opposite direction
are clear.
So we can step that direction, check to the left and
right of us, then step in the opposite direction
and do the same. This allows us to check the 4
blocks that touch the corners of the center block.
*/
def countDiagonalBlockages =
    // First, orient to the clear front-to-back path
    amBlocked <- isBlockedOrFenced;
    if amBlocked {turn left;} {};

    // Second, step to both sides
    fwdCount <- observeLeftAndRight;
    backCount <- observeLeftAndRight;
    return $ fwdCount + backCount;
    end;

def isStandingOnBridge =
    onFence <- ishere "fence";
    onGate <- ishere "gate";
    if (onFence || onGate) {
        adjCount <- countAdjacentBlockages;
        if (adjCount > 1) {
            return true;
        } {
            diagCount <- countDiagonalBlockages;
            return $ (adjCount + diagCount) > 1;
        };
    } {return false};
    end;

def getValForSheepIndex = \predicateCmd. \i.
    try {
        // This will throw an exception if
        // the sheep has already drowned.
        r <- robotnumbered i;
        didSucceed <- as r {predicateCmd};

        boolToInt didSucceed;
    } {
        return 0;
    }
    end;

/**
There are 3 sheep.
They have indices 1, 2, 3.
(The base has index 0).

THIS DOES NOT WORK!
*/
def countSheepWithRecursive = \predicateCmd. \i.

    if (i > 0) {
        val <- getValForSheepIndex predicateCmd i;
        recursiveCount <- countSheepWithRecursive predicateCmd $ i - 1;
        return $ val + recursiveCount;
    } {
        return 0;
    }
    end;


def countSheepWith = \predicateCmd.

    val1 <- getValForSheepIndex predicateCmd 1;
    val2 <- getValForSheepIndex predicateCmd 2;
    val3 <- getValForSheepIndex predicateCmd 3;
    return $ val1 + val2 + val3;

    end;


justFilledGap <- as base {
    isStandingOnBridge;
};

if (justFilledGap) {
    enclosedCount <- countSheepWith checkIsEnclosed;
    return $ enclosedCount >= 1;
} {
    return false;
}
