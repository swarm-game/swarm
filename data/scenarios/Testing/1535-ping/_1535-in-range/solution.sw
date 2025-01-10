def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def goToBuddy = \loc.

    // log $ format loc;
    
    let longitudinalDist = snd loc in
    absFwd <- if (longitudinalDist < 0) {
        turn back;
        pure $ -longitudinalDist;
    } {
        pure longitudinalDist;
    };
    doN absFwd move;
    if (longitudinalDist < 0) {
        turn back;
    } {};

    let lateralDist = fst loc in
    absSide <- if (lateralDist < 0) {
        turn left;
        pure $ -lateralDist;
    } {
        turn right;
        pure lateralDist;
    };
    doN absSide move;
    end;

def checkNeedToMove = \f. \loc.
    wait 3;
    if (loc == (0, 0)) {
        pure ()
    } {
        goToBuddy loc;
        f;
    }
    end;

def pingLoop = \buddy.
    maybeLoc <- ping buddy;
    case maybeLoc pure $ checkNeedToMove $ pingLoop buddy;
    end;

def giveToBuddy = \buddy.
    give buddy "map piece";
    pingLoop buddy;
    end;

def go =
    move;
    maybeBuddy <- meet;
    case maybeBuddy pure giveToBuddy;
    grab;
    end;

go;