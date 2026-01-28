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

def goToBottom =
    turn south; doN 14 move;
    end;

def go =
    goToLocation (0, 0) (3, -2);
    goToLocation (0, 0) (12, -2);
    goToLocation (0, 0) (18, -5);
    goToLocation (0, 0) (23, -3);

    goToBottom;
    goToLocation (0, -14) (3, -12);
    goToBottom;
    goToLocation (0, -14) (9, -9);
    goToBottom;
    goToLocation (0, -14) (18, -9);
    goToBottom;
    goToLocation (0, -14) (26, -10);

    turn east;
    doN 29 move;
    goToBottom;
    grab;
    end;
