import "~swarm/lib/control"
import "~swarm/lib/list"

def isEnclosureFull : Int * Int -> Cmd Bool = \encl.
    prevLoc <- whereami;

    dims <- floorplan "rubbish enclosure";
    teleport self encl;

    c <- density ((0, 0), dims);
    match dims \w. \h.
    let area = w * h in
    let notFull = c < area in

    teleport self prevLoc;
    pure $ not notFull;
    end;

def isEitherEnclosureFull =
    enclosures <- structures "rubbish enclosure";
    any isEnclosureFull enclosures
    end;

def tryGrab =
    try {
        grab;
        pure ()
    } {};
    end;


def turnAround = \d.
    intersperse 2 move $ turn d;
    end;

def waitUntilEnclosureFull =

    isFull <- instant {isEitherEnclosureFull};
    if isFull {
        // Drive down the road
        turn south;
        doN 13 move;
        turn right;
        doN 3 move;
        turn right;
        doN 2 move;
        turn left;

        // North enclosure
        intersperse 2 (turnAround left) $ intersperse 3 move tryGrab;

        intersperse 2 (doN 3 move) $ turn right;

        // South enclosure
        intersperse 2 (turnAround right) $ intersperse 3 move tryGrab;

        turn left;
        move;

        // Leave again
        turn right;
        doN 3 move;
        turn left;
        doN 13 move;
        turn back;
    } {
        wait 10;
        waitUntilEnclosureFull;
    }
    end;

def go =
    waitUntilEnclosureFull;
    go;
    end;
