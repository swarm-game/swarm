def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def abs = \n. if (n < 0) {-n} {n} end;

def λmatch = \f. \p. match p f end

def mapTuple = \f. λmatch \a. \b. (f a, f b) end;

def sumTuples = λmatch \t11. \t12. λmatch \t21. \t22.
    (t11 + t21, t12 + t22);
    end;

def negateTuple = \t.
    mapTuple (\x. -x) t;
    end;

def getRelativeLocation = \absCurrentLoc. \absDestLoc.
    let negatedLoc = negateTuple absCurrentLoc in
    pure $ sumTuples negatedLoc absDestLoc;
    end;

def moveTuple = λmatch \x. \y.
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

    case myList pure (λmatch \hd. \tl.
        goToLocation loc hd;
        visitNextWaypoint originalList tl;
    );
    end;

def go =
    let wpList = waypoints "wp" in
    visitNextWaypoint wpList wpList;
    end;

go;
