import "~swarm/lib/control"
import "~swarm/lib/arith"
import "~swarm/lib/tuple"

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
