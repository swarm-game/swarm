import "~swarm/lib/control"
import "common"

def waitUntilHere = \remainingCount.
    if (remainingCount > 0) {
        maybeItemHere <- scan down;
        case maybeItemHere (\_. 
            watch down;
            wait 1000;
            waitUntilHere remainingCount;
        ) (\itemHere.

            maybeItemAbove <- scan left;
            case maybeItemAbove (\_. fail "Expected an item here.") (\itemAbove.
                let num = getNumberForBase itemAbove in
                if (num >= 0) {
                    let complementNum = getComplementNumber num in
                    let complementItem = getBaseForNumber complementNum in
                    if (complementItem == itemHere) {
                        move;
                        waitUntilHere $ remainingCount - 1;
                    } {
                        create "pixel (R)";
                    }
                } {
                    fail "Expected nonnegative item index."
                }
            );
        );
    } {
        create "pixel (G)";
    };
    end;

def waitUntilEmpty =
    watch down;
    wait 1000;
    emptyHere <- isempty;
    if emptyHere {} {
        waitUntilEmpty;
    }
    end;

def waitForReset =
    backup;
    waitUntilEmpty;
    end;

def go = \startingLoc.
    instant {waitUntilHere 32};
    waitForReset;
    teleport self startingLoc;
    go startingLoc;
    end;

def topchecker =
  loc <- whereami;
  go loc;
end
