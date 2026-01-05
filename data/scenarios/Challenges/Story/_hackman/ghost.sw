/* Algorithm:
Go forward until along any straight passage until reaching
an intersection. Then randomly choose any available direction
(except backwards).
*/

def isItemInDirection = \direction. \item.
    x <- scan direction;
    pure $ case x (\_. false) (\y. y == item);
    end;

// A ghost is not blocked by a "gate" when it is leaving the "nursery".
def canCrossGate =
    isGate <- isItemInDirection forward "gate";
    loc <- whereami;
    let amCentered = loc == (0, 0) in
    pure $ isGate && amCentered;
    end;

def isBlockedAhead =
    isBlockedByWall <- blocked;
    ghostCanCrossGate <- canCrossGate;
    pure $ isBlockedByWall && not ghostCanCrossGate;
    end;

def checkLeftBlocked =
    turn left;
    isBlocked <- isBlockedAhead;
    turn right;
    pure isBlocked;
    end;

def checkRightBlocked =
    turn right;
    isBlocked <- isBlockedAhead;
    turn left;
    pure isBlocked;
    end;

def chooseDirection : Cmd Dir =
    leftBlocked <- checkLeftBlocked;
    rightBlocked <- checkRightBlocked;
    forwardBlocked <- isBlockedAhead;
    if (leftBlocked && rightBlocked && forwardBlocked) {
        say "Dead end; turning back";
        pure back;
    } {
        // Since we're in the "else" case of all three
        // being blocked, we know that at least one
        // direction is available. So we check all
        // the combinations.
        if (leftBlocked && rightBlocked) {
            // Keep going straight
            pure forward;
        } {
            if (leftBlocked && forwardBlocked) {
                pure right;
            } {
                if (rightBlocked && forwardBlocked) {
                    pure left;
                } {
                    // Now we have exhaused all of the combinations
                    // of *two* directions being blocked, so we
                    // check single directions.

                    if leftBlocked {
                        // Decide whether to go right or straight
                        d <- random 2;
                        if (d == 0) {
                            pure right;
                        } {
                            // go straight
                            pure forward;
                        }
                    } {
                        if rightBlocked {
                            // Decide whether to go left or straight
                            d <- random 2;
                            if (d == 0) {
                                pure left;
                            } {
                                // go straight
                                pure forward;
                            }
                        } {
                            if forwardBlocked {
                                // Decide whether to go left or right
                                d <- random 2;
                                if (d == 0) {
                                    pure left;
                                } {
                                    // go right
                                    pure right;
                                }
                            } {
                                // No directions are blocked, so we can
                                // go left, right, or straight with
                                // equal probability.

                                d <- random 3;
                                if (d == 0) {
                                    pure left;
                                } {
                                    if (d == 1) {
                                        pure right;
                                    } {
                                        // go straight
                                        pure forward;
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

def go = \startPos.
    hasGift <- has "donut";
    if hasGift {
        teleport self startPos;
    } {
        move;
        newDirection <- chooseDirection;
        turn newDirection;
        go startPos;
    }
    end;

def waitToStart =
    startPos <- whereami;
    offset <- random 300;
    let startDelay = 50 + offset in
    wait startDelay;
    teleport self (0, 0);
    go startPos;
    end;
