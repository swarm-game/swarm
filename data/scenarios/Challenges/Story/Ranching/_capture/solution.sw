/**
Need to synchronize pushes so that in the same tick
that the first boulder comes into contact with the
opponent, there is no escape route.
*/

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def walkAround = \d1. \d2.
    turn d1;
    doN 3 move;
    turn d2;

    doN 3 move;
    turn d2;
    end;

def buildBot = \initialTurn.
    _r1 <- build {
        watch back;
        wait 200;
        doN 2 move;
        turn initialTurn;
        walkAround right left;
        push;
    };

    _r2 <- build {
        watch back;
        wait 200;
        doN 2 move;
        turn initialTurn;
        walkAround left right;
        push;
    };
    end;

def arrangeBoulders =
    // First boulder
    turn right;
    move;
    turn left;
    doN 7 move;
    turn left;
    push;
    turn right;
    move;
    turn left;
    move;
    turn left;
    doN 4 push;

    // Second boulder
    turn back;
    doN 7 move;
    turn left;
    doN 2 move;
    turn left;
    doN 5 push;

    // Third boulder
    turn right;
    doN 4 move;
    turn left;
    doN 7 move;
    turn left;
    move;
    turn left;
    doN 3 push;
    turn left;
    move;
    turn right;
    move;
    turn right;
    push;

    // Fourth boulder
    turn right;
    doN 3 move;
    turn left;
    doN 2 push;
    turn right;
    move;
    turn left;
    move;
    turn left;
    push;
    end;

def go =

    doN 3 move;
    obj <- grab;

    arrangeBoulders;

    turn right;
    doN 2 move;
    turn back;
    log "building bots 1!"; // 4,-6
    buildBot right;
    
    // walk around the watched place
    turn right;
    move;
    turn right;
    move;
    turn left;

    log "building bots 2!";
    buildBot left;
    turn back;
    wait 20;
    log "going back now!";
    move;

    place obj;
    move;
    end;

go;