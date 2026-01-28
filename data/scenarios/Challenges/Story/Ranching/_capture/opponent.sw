import "~swarm/lib/control"
import "~swarm/lib/arith"

/**
If any blocking entity is touching me, escape.
*/

def getDirection = \n.
    if (n == 0) {
        forward;
    } $ elif (n == 1) {
        right;
    } $ elif (n == 2) {
        back;
    } $ elif (n == 3) {
        left;
    } $ else {
        down;
    };
    end;

def forDirs = \c.
  forwardRes <- c forward;
  rightRes <- c right;
  backRes <- c back;
  leftRes <- c left;
  pure (forwardRes, rightRes, backRes, leftRes)
  end;

def watchDir = \n.
    watch $ getDirection n;
    if (n > 0) {
        watchDir $ n - 1;
    } {};
    end;

def boolToInt = \b.
    if b {1} {0}
    end;

/**
Iterate 4 times (once for each direction)
*/
def countBlocked = \n.
    if (n > 0) {
        isBlocked <- blocked;
        turn left;
        lastCount <- countBlocked $ n - 1;
        pure $ lastCount + boolToInt isBlocked;
    } {
        pure 0;
    }
    end;

def reWatch =
    watchDir 4;
    end;

def reWatchEntities = \instantCont.
    s1 <- instant {
        forDirs (\d. watch d; scan d)
    };
    wait 10000;
    instant {
        s2 <- forDirs scan;
        if (s1 != s2) {instantCont} {}
    };
    reWatchEntities instantCont
    end

def locationIsOpen =
    emptyHere <- isempty;
    blockedCount <- countBlocked 4;
    pure $ emptyHere && blockedCount == 0;
    end;

def faceAwayFrom = \loc.
    match loc \locx. \locy.
    myLoc <- whereami;
    match myLoc \myx. \myy.
    let x = locx - myx in
    let y = locy - myy in
    let d = if (abs x > abs y) {
        if (x > 0) {west} {east}
    } {
        if (y > 0) {south} {north}
    } in
    turn d;
    end;

/**
Move in opposite direction from base to
find a cross-shaped region that has zero blockages.
*/
def findOpenArea =
    baseLoc <- as base {whereami};
    faceAwayFrom baseLoc;
    until locationIsOpen move;
    end;

def relocateAway =
    marker <- grab;
    findOpenArea;
    try {
        swap marker;
        pure ();
    } {
        place marker;
    };
    end;

def handleBlockage =
    blockedCount <- countBlocked 4;
    if (blockedCount > 0) {
        if (blockedCount < 4) {
            relocateAway;
        } {
            selfdestruct;
        }
    } {};
    end;

def go =
    reWatchEntities handleBlockage;
    go;
    end;
