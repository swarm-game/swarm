def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def isEnclosureFull : Int * Int -> Cmd Bool = \encl.
    prevLoc <- whereami;

    dims <- floorplan "rubbish enclosure";
    teleport self encl;

    c <- density ((0, 0), dims);
    let area = fst dims * snd dims in
    let notFull = c < area in

    teleport self prevLoc;
    pure $ not notFull;
    end;

def any : (a -> Cmd Bool) -> (rec l. Unit + a * l) -> Cmd Bool = \p. \l.
  case l
    (\_. pure false)
    (\c. b <- p (fst c); if b {pure true} {any p (snd c)})
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

    isFull <- instant isEitherEnclosureFull;
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

go;
