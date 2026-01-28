import "~swarm/lib/control"
import "common"

def placeComplementOf = \item.
    let baseNumber = getNumberForBase item in
    if (baseNumber >= 0) {
        let complementNumber = getComplementNumber baseNumber in
        let newItem = getBaseForNumber complementNumber in
        move;
        create newItem;
        place newItem;
    } {
        let sludge = "organic sludge" in
        create sludge;
        place sludge;
    }
    end;

def waitUntilHere =
    watch down;
    wait 1000;

    maybeItemDown <- scan down;
    case maybeItemDown (\_. waitUntilHere) (\itemHere.
        placeComplementOf itemHere;
    );
    end;

def waitUntilEmpty =
    watch down;
    wait 1000;
    emptyHere <- isempty;
    if emptyHere {
        // reset the position
        turn back;
        move;
        turn back;
    } {
        waitUntilEmpty;
    }
    end;

def go =
    instant {waitUntilHere};
    waitUntilEmpty;
    go;
    end;
