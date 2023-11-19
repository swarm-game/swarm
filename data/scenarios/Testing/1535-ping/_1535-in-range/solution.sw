def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def goToBuddy = \loc.

    // log $ format loc;
    
    let longitudinalDist = snd loc in
    absFwd <- if (longitudinalDist < 0) {
        turn back;
        return $ -longitudinalDist;
    } {
        return longitudinalDist;
    };
    doN absFwd move;
    if (longitudinalDist < 0) {
        turn back;
    } {};

    let lateralDist = fst loc in
    absSide <- if (lateralDist < 0) {
        turn left;
        return $ -lateralDist;
    } {
        turn right;
        return lateralDist;
    };
    doN absSide move;
    end;

def checkNeedToMove = \f. \loc.
    wait 3;
    if (loc == (0, 0)) {
        return ()
    } {
        goToBuddy loc;
        f;
    }
    end;

def pingLoop = \buddy.
    maybeLoc <- ping buddy;
    case maybeLoc return $ checkNeedToMove $ pingLoop buddy;
    end;

def giveToBuddy = \buddy.
    give buddy "map piece";
    pingLoop buddy;
    end;

def go =
    move;
    maybeBuddy <- meet;
    case maybeBuddy return giveToBuddy;
    grab;
    end;

go;