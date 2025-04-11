def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def abs = \n. if (n < 0) {-n} {n} end;

def mapTuple = \f. \t.
    (f $ fst t, f $ snd t)
    end;

def sumTuples = \t1. \t2.
    (fst t1 + fst t2, snd t1 + snd t2);
    end;

def negateTuple = \t.
    mapTuple (\x. -x) t;
    end;

def getRelativeLocation = \absCurrentLoc. \absDestLoc.
    let negatedLoc = negateTuple absCurrentLoc in
    pure $ sumTuples negatedLoc absDestLoc;
    end;

def moveTuple = \tup.
    let x = fst tup in
    let y = snd tup in
    turn $ if (x > 0) {east} {west};
    doN (abs x) move;
    turn $ if (y > 0) {north} {south};
    doN (abs y) move;
    end;

def goToLocation = \currentLoc. \absoluteDestination.
    relativeDestination <- getRelativeLocation currentLoc absoluteDestination;
    moveTuple relativeDestination;
    end;

def visitNextWaypoint : (rec l. Unit + (Int * Int) * l) -> (rec l. Unit + (Int * Int) * l) -> Cmd Unit = \originalList. \remainingList.
    loc <- whereami;

    // Wrap around
    let myList = case remainingList (\_. originalList) (\_. remainingList) in

    case myList pure (\cons.
        goToLocation loc $ fst cons;
        visitNextWaypoint originalList $ snd cons;
    );
    end;

def go =
    let wpList = waypoints "wp" in
    visitNextWaypoint wpList wpList;
    end;

go;
