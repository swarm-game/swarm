/* Algorithm:
Go forward until along any straight passage until reaching
an intersection. Then randomly choose any available direction
(except backwards).
*/

def isItemInDirection = \direction. \item.
    x <- scan direction;
    return $ case x (\_. false) (\y. y == item);
    end;

// A ghost is not blocked by a "gate" when it is leaving the "nursery".
def canCrossGate =
    isGate <- isItemInDirection forward "gate";
    loc <- whereami;
    let amCentered = loc == (0, 0) in
    return $ isGate && amCentered;
    end;

def isBlockedAhead =
    isBlockedByWall <- blocked;
    ghostCanCrossGate <- canCrossGate;
    return $ isBlockedByWall && not ghostCanCrossGate;
    end;

def checkLeftBlocked =
    turn left;
    isBlocked <- isBlockedAhead;
    turn right;
    return isBlocked;
    end;

def checkRightBlocked =
    turn right;
    isBlocked <- isBlockedAhead;
    turn left;
    return isBlocked;
    end;

def chooseDirection : cmd dir =
    leftBlocked <- checkLeftBlocked;
    rightBlocked <- checkRightBlocked;
    forwardBlocked <- isBlockedAhead;
    if (leftBlocked && rightBlocked && forwardBlocked) {
        say "Dead end; turning back";
        return back;
    } {
        // Since we're in the "else" case of all three
        // being blocked, we know that at least one
        // direction is available. So we check all
        // the combinations.
        if (leftBlocked && rightBlocked) {
            // Keep going straight
            return forward;
        } {
            if (leftBlocked && forwardBlocked) {
                return right;
            } {
                if (rightBlocked && forwardBlocked) {
                    return left;
                } {
                    // Now we have exhaused all of the combinations
                    // of *two* directions being blocked, so we
                    // check single directions.

                    if leftBlocked {
                        // Decide whether to go right or straight
                        d <- random 2;
                        if (d == 0) {
                            return right;
                        } {
                            // go straight
                            return forward;
                        }
                    } {
                        if rightBlocked {
                            // Decide whether to go left or straight
                            d <- random 2;
                            if (d == 0) {
                                return left;
                            } {
                                // go straight
                                return forward;
                            }
                        } {
                            if forwardBlocked {
                                // Decide whether to go left or right
                                d <- random 2;
                                if (d == 0) {
                                    return left;
                                } {
                                    // go right
                                    return right;
                                }
                            } {
                                // No directions are blocked, so we can
                                // go left, right, or straight with
                                // equal probability.

                                d <- random 3;
                                if (d == 0) {
                                    return left;
                                } {
                                    if (d == 1) {
                                        return right;
                                    } {
                                        // go straight
                                        return forward;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    end;

// Ghosts must implement teleporting logic themselves.
// Hard-codes location of the teleporter.
def checkNeedsTeleport = \offset.
    currentLoc <- whereami;
    if (fst currentLoc < -offset) {
        teleport self (offset, snd currentLoc)
    } {
        if (fst currentLoc > offset) {
            teleport self (-offset, snd currentLoc)
        } {}
    }
    end;

startPos <- whereami;

def go =
    hasGift <- has "donut";
    if hasGift {
        teleport self startPos;
    } {
        move;
        checkNeedsTeleport 11;
        newDirection <- chooseDirection;
        turn newDirection;
        go;
    }
    end;

def waitToStart =
    offset <- random 300;
    let startDelay = 50 + offset in
    wait startDelay;
    teleport self (0, 0);
    go;
    end;

waitToStart;