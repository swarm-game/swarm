def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def makeRoll =
    make "nori";
    make "california roll";
    end;

def checkIngredients =
    hasTuna <- has "crab";
    hasSeaweed <- has "seaweed";
    pure $ hasTuna && hasSeaweed;
    end;

def catchFish = \rod.
    use rod forward;
    ready <- checkIngredients;
    if ready {
        makeRoll;
    } {
        catchFish rod;
    };
    end;

def turnAround = \d.
    intersperse 2 move $ turn d;
    end;

/*
Precondition:
At the top-right corner
*/
def harvestRectangle =
    intersperse 4 move $ harvest; pure ();
    turnAround left;
    intersperse 4 move $ harvest; pure ();
    end;

def harvestIngredients =
    turn north;
    doN 2 move;
    turn left;
    doN 3 move;

    intersperse 3 (turn right; doN 2 move; turn right;) harvestRectangle;
    wait 400;
    turn left;
    doN 7 move;
    turn left;
    intersperse 3 (turn right; doN 2 move; turn right;) harvestRectangle;

    doN 6 move;
    turn left;
    move;
    end;

def find : (a -> Cmd Bool) -> (rec l. Unit + a * l) -> Cmd (Unit + a) = \p. \l.
  case l
    (\_. pure (inl ()))
    (\cons. h <- p (fst cons); if h {pure $ inr (fst cons)} {find p (snd cons)})
end

def tryPlace = \item.
    try {
        place item;
    } {};
    end;

/**
Precondition: facing north in lower-left corner of enclosure

Navigates a serpentine pattern through the space to
place items.
*/
def placeSerpentine = \placeFunc.
    placeFunc;

    move;
    placeFunc;

    turn right;

    move;
    placeFunc;

    turn right;

    move;
    placeFunc;

    turn left;

    move;
    placeFunc;

    turn left;

    move;
    placeFunc;

    end;

def pureToCorner =
    turn back;
    move; move;
    turn right;
    move; move;
    turn right;
    end;

def unloadTrash =
    try {
        placeSerpentine (
            item <- find has (tagmembers "junk");
            case item (\_. fail "done") (\item. place item);
        );
        watch down;
        wait 1000;

        // Go back to corner
        turn back;
        move;
        turn right;
        move; move;
        turn right;

        wait 50;
        unloadTrash;
    } {};
    end;

def burnTires =
    hasCarTire <- has "car tire";
    if hasCarTire {

        intersperse 2 move $ (
            placeSerpentine $ tryPlace "car tire";
            pureToCorner;
            ignite forward;

            turn right;
            move; move;
            turn right;
        );

        wait 60;
        move;

        burnTires;
    } {}
    end;

def disposeTrash =
    turn back;
    doN 5 move;
    turn left;
    doN 11 move;
    turn left;
    move;

    burnTires;

    unloadTrash;
    end;

def go =
    harvestIngredients;
    let rod = "fishing tackle" in
    make rod;
    equip rod;
    doN 16 $ catchFish rod;
    disposeTrash;
    end;

go;
