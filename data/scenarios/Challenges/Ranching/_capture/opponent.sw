/**
If any blocking entity is touching me, escape.
*/

def elif = \t. \then. \else. {if t then else} end
def else = \t. t end
def abs = \n. if (n < 0) {-n} {n} end;
def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def until = \p. \c. q <- p; if q {} {c; until p c} end;

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
        // This assignment has to happen before the recursive
        // "countBlocked" call due to #1032
        let localBlockCount = boolToInt isBlocked in
        turn left;
        lastCount <- countBlocked $ n - 1;
        return $ lastCount + localBlockCount;
    } {
        return 0;
    }
    end;

def reWatch =
    watchDir 4;
    end;

def locationIsOpen =
    emptyHere <- isempty;
    blockedCount <- countBlocked 4;
    return $ emptyHere && blockedCount == 0;
    end;

def faceAwayFrom = \loc.
    myLoc <- whereami;
    let x = fst loc - fst myLoc in
    let y = snd loc - snd myLoc in
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
        return ();
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
    instant reWatch;
    wait 1000;
    instant handleBlockage;
    go;
    end;

go;