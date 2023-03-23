def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def tryGrab =
    try {
        grab;
        return ();
    } {};
    end;

def tryGive = \thing.
    hasPowerup <- has thing;
    if hasPowerup {
        r <- meet;
        case r
            (\_. return ())
            (\r. give r thing; wait 10);
    } {};
    end;

def turnAndEat = \d. \c.
    turn d;
    doN c (move; tryGrab; tryGive "donut");
    end;

def eatAllPellets =

    doN 4 move;
    turn right;
    doN 2 move;

    tryGrab;
    turnAndEat right 3;
    turnAndEat left 2;
    turnAndEat left 5;
    turnAndEat right 2;
    turnAndEat left 4;
    turnAndEat right 2;
    turnAndEat right 20;
    turnAndEat right 2;
    turnAndEat right 4;
    turnAndEat left 4;
    turnAndEat right 5;
    turnAndEat right 2;
    turnAndEat right 3;
    turnAndEat left 2;
    turnAndEat left 3;
    turnAndEat right 2;
    turnAndEat left 2;
    turnAndEat left 2;
    turnAndEat right 3;
    turnAndEat left 2;
    turnAndEat left 10;
    turnAndEat left 2;
    turnAndEat right 2;
    turnAndEat right 2;
    turnAndEat left 2;
    turnAndEat right 2;
    turnAndEat right 4;
    turnAndEat left 13;
    turnAndEat right 5;
    turnAndEat right 3;
    turnAndEat right 9;
    turnAndEat right 3;
    turnAndEat right 4;
    turnAndEat right 5;
    turnAndEat right 4;
    turnAndEat right 2;
    turnAndEat right 6;
    turnAndEat right 2;
    turnAndEat left 3;
    turnAndEat right 2;
    turnAndEat left 2;
    turnAndEat left 2;
    turnAndEat right 3;
    turnAndEat left 2;
    turnAndEat left 4;
    turnAndEat back 1;
    turnAndEat left 3;
    turnAndEat right 5;
    turnAndEat right 3;
    turnAndEat left 4;
    turnAndEat left 3;
    turnAndEat left 4;
    turnAndEat left 3;
    turnAndEat right 1;
    turnAndEat back 5;
    turnAndEat right 2;
    turnAndEat right 4;
    turnAndEat right 1;
    turnAndEat back 9;
    turnAndEat right 1;
    turnAndEat back 1;
    turnAndEat right 1;
    turnAndEat back 1;
    turnAndEat right 4;
    turnAndEat right 2;
    turnAndEat right 2;
    turnAndEat left 1;
    end;

def waitToGive =
    tryGive "donut";
    hasDonut <- has "donut";
    if hasDonut {waitToGive} {};
    end;

def returnToCenter =
    turn back;
    move;
    turn right;
    doN 2 move;
    turn left;
    doN 2 move;
    turn left;
    doN 6 move;
    turn right;
    doN 2 move;
    turn left;
    doN 4 move;
    _strawberry <-grab;
    doN 4 move;
    turn right;
    doN 2 move;
    turn left;
    doN 2 move;
    end;

def invadeDen =
    doN 14 move;
    turn right;
    doN 2 move;
    turn left;
    doN 4 move;
    turn left;
    make "den key";
    drill forward;
    doN 2 move;
    end;

eatAllPellets;
returnToCenter;
waitToGive;
invadeDen;