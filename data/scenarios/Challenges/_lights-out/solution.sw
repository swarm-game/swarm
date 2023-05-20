def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

/** Precondition: facing "d" direction */
def toggleToDark = \d.
    onHere <- ishere "on";
    if onHere {drill d; return ()} {};
    end;

def visitSingleRow = \rowWidth. \d.
    intersperse rowWidth move $ toggleToDark d;
    end;

def turnAround = \d.
    turn d;
    move;
    turn d;
    end;

/** Iterates through the first four rows. */
def chaseUpperLights = \rowWidth.
    visitSingleRow rowWidth right;
    turnAround right;
    visitSingleRow rowWidth left;
    turnAround left;
    visitSingleRow rowWidth right;
    turnAround right;
    visitSingleRow rowWidth left;
    end;

/**
Place yourself in the upper-left corner, facing east.
*/
def goToCorner =
    move;
    turn right;
    move;
    turn left;
    end;

def onInDirection = \d.
    entHere <- scan d;
    return $ case entHere (\_. false) (\e. e == "on");
    end;

/**
If the light at A5 is on, press D1 and E1.
*/
def fixA5 =
    turn back;
    doN 2 move;
    turn right;
    doN 3 move;
    drill left;
    move;
    drill left;
    turn back;
    doN 3 move;
    turn left;
    doN 2 move;
    end;

/**
If the light at B5 is on, press B1 and E1.
*/
def fixB5 =
    turn back;
    doN 2 move;
    drill forward;
    turn right;
    doN 3 move;
    drill left;
    turn back;
    doN 2 move;
    turn left;
    doN 2 move;
    end;

/**
If the light at C5 is on, press D1.
*/
def fixC5 =
    turn back;
    doN 3 move;
    turn right;
    drill forward;

    turn back;
    doN 2 move;
    turn back;
    end;

/**
Precondition: on first column of fourth row, facing south.
*/
def fixUpperLights =
    a5on <- onInDirection forward;
    if a5on {fixA5} {
        turn left;
        move;
        turn right;
    };

    b5on <- onInDirection forward;
    if b5on {fixB5} {
        turn left;
        move;
        turn right;
    };

    c5on <- onInDirection forward;
    if c5on {fixC5} {
        turn right;
        doN 2 move;
        turn right;
        doN 3 move;
        turn right;
    };


    end;

def go =
    goToCorner;
    chaseUpperLights 5;
    turn left;
    fixUpperLights;
    chaseUpperLights 5;
    end;

go;