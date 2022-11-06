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

def isFenced =
    s <- scan forward;
    return (
        case s
            (\_. false)
            (\x. x == "fence" || x == "gate" || x == "solid fence")
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
            case x return (\_. grab; return ());

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

def checkIsEnclosed =
    try {
        goForwardToPatrol false;
        // Water is the outer boundary
        hasWater <- ishere "water";
        if hasWater {
            return false;
        } {
            checkIsEnclosed;
        };
    } {
        return true;
    };
    end;

checkIsEnclosed;