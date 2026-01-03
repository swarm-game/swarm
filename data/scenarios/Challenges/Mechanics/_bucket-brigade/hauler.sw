import "~swarm/lib/control"

/** The function argument returns a boolean.
Keep running the function until either the
loop counter runs out or the function returns true.
*/
def doNTimesOr = \n. \f.
    if (n > 0) {
        shouldQuit <- f;
        if shouldQuit {} {
            doN (n - 1) f;
        }
    } {};
    end;

def traverseRoad =
    doN 11 (
        move;
        wait 7;
    );
    end;

def patrol =
    let briquette = "coal briquette" in
    traverseRoad;
    doNTimesOr 40 (
        briquetteIsHere <- ishere briquette;
        if briquetteIsHere {grab; pure ()} {};
        pure briquetteIsHere;
    );
    turn back;
    traverseRoad;

    hasBriquette <- has briquette;
    if hasBriquette {
        say "Delivered!";
        make "energy";
    } {};
    turn back;
    end;

def hauler = forever patrol end
