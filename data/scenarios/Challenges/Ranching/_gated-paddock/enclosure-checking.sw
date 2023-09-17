def isBlockedOrFenced =
    b <- blocked;
    return b;
    end;

def checkIsEnclosed =
    maybePath <- path (inL ()) (inR "water");
    case maybePath (\_. return True) (\_. return False);
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
