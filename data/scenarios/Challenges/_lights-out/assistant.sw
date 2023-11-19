def elif = \t. \then. \else. {if t then else} end
def else = \t. t end
def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def boolToInt = \b.
    if b {1} {0}
    end;

// modulus function (%)
def mod : int -> int -> int = \i.\m.
  i - m * (i / m)
end

def isEven = \n.
    mod n 2 == 0;
    end

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def sumTuples = \t1. \t2.
    (fst t1 + fst t2, snd t1 + snd t2);
    end;

def mapTuple = \f. \t.
    (f $ fst t, f $ snd t)
    end;

def replaceWith = \withThis.
    create withThis;
    swap withThis;
    return ();
    end;

/** Modifies the cell */
def invertLight = \e.
    if (e == "off") {
        replaceWith "on";
    } $ elif (e == "on") {
        replaceWith "off";
    } {}
    end;

def toggleLightHere =
    entHere <- scan down;
    case entHere return invertLight;
    end;

/** Precondition: in the middle of a "cross" */
def toggleSingleNeighbor =
    move;
    toggleLightHere;
    turn back;
    move;
    end;

def toggleAllNeighbors =
    doN 2 (
        doN 2 toggleSingleNeighbor;
        turn left;
    );
    end;

def flipSelfAndNeighbors = \newState. \locOffset.
    curLoc <- whereami;
    let newLoc = sumTuples locOffset curLoc in
    teleport self newLoc;
    replaceWith newState;
    toggleAllNeighbors;
    teleport self curLoc;
    end;

def togglePending = \state.
    let pendingEntityName = "pending-" ++ state in
    maybePending <- detect pendingEntityName ((1, 1), (6, 6));
    case maybePending return $ flipSelfAndNeighbors state;
    end;

def observe =
    instant (
        togglePending "on";
        togglePending "off";
    );

    // Without this 'wait' call, we may end up doing the instant call
    // multiple times in one tick, until it uses up its step allotment.
    // We really only want the 'observe' function to execute once per tick.
    wait 1;

    observe;
    end;

def makeOnIf = \b.
    if b {replaceWith "on"} {};
    end;

/** Precondition: Light is off */
def randomOn =
    x <- random 2;
    makeOnIf $ x == 0;
    end;

/**
This is a distillation into code of the
first quiet pattern here:
https://www.jaapsch.net/puzzles/lights.htm#quiet

10101
10101
00000
10101
10101

Note that the second quiet pattern is just the transpose,
so we can simply swap the position index arguments to obtain it:

11011
00000
11011
00000
11011

Indices are zero-based.
*/
def isQuietPatternMember = \rowIdx. \colIdx.
    rowIdx != 2 && mod colIdx 2 == 0;
    end;

def advanceRowViaTeleport =
    curLoc <- whereami;
    teleport self (0, snd curLoc - 1);
    end;

def shouldCorrectTile : (bool * bool) -> (bool * bool) -> cmd bool = \evenOverlaps. \isQuietTiles.
    if (evenOverlaps == isQuietTiles) {
        toggleLightHere;
        return true;
    } {
        return false;
    }
    end;

/** Returns the number of lights in common
with each quiet pattern, for this row.
*/
def prepareBoardRow = \abortFunc. \rowIdx. \colIdx.
    if (colIdx >= 0) {

        isCurrentlyOn <- ishere "on";

        let isQuietTile1 = isQuietPatternMember rowIdx colIdx in
        let isQuietTile2 = isQuietPatternMember colIdx rowIdx in

        let quietTuple = (isQuietTile1, isQuietTile2) in

        shouldAbort <- abortFunc quietTuple;
        if shouldAbort {
            return ((0, 0), true);
        } {
            let quietCellOn = mapTuple (\x. x && isCurrentlyOn) quietTuple in
            let addend = mapTuple boolToInt quietCellOn in

            move;
            retval <- prepareBoardRow abortFunc rowIdx $ colIdx - 1;
            let subTotal = fst retval in
            return $ (sumTuples addend subTotal, snd retval);
        }
    } {
        return ((0, 0), false);
    }
    end;

/** Returns the number of lights in common
with each quiet pattern.
*/
def prepareBoardAllRows = \abortFunc. \boardWidth. \rowIdx.
    if (rowIdx >= 0) {
        retval <- prepareBoardRow abortFunc rowIdx $ boardWidth - 1;
        let rowCommonCount = fst retval in
        let shouldAbort = snd retval in

        if shouldAbort {
            return (0, 0);
        } {
            advanceRowViaTeleport;

            // This reassignment has to happen before the recursive
            // "prepareBoardAllRows" call due to #1032
            let rowCommonCountFoo = rowCommonCount in
            totalCommonCount <- prepareBoardAllRows abortFunc boardWidth $ rowIdx - 1;

            return $ sumTuples rowCommonCountFoo totalCommonCount
        }
    } {
        return (0, 0);
    }
    end;

def checkIsSolvable = \boardWidth. \boardHeight.
    overlapCounts <- prepareBoardAllRows (\_. return false) boardWidth $ boardHeight - 1;
    // say $ "Overlap counts: " ++ format overlapCounts;
    return $ mapTuple isEven overlapCounts;
    end;

/** Teleports to a new location to execute a function
  then returns to the original location before
  returning the functions output value.
*/
def atLocation = \newLoc. \f.
    prevLoc <- whereami;
    teleport self newLoc;
    retval <- f;
    teleport self prevLoc;
    return retval;
    end;

def analyzeSolvability : int -> int -> cmd (bool * bool) = \boardWidth. \boardHeight.
    atLocation (0, 0) $
        checkIsSolvable boardWidth boardHeight;
    end;

def prepareBoardRandom = \boardWidth. \boardHeight. 
    atLocation (0, 0) $
        intersperse boardHeight advanceRowViaTeleport $
            intersperse boardWidth move randomOn;
    end;

def ensureSolvability = \evenOverlaps. \boardWidth. \boardHeight.
    let isSolvable = fst evenOverlaps && snd evenOverlaps in
    // say $ "isSolvable: " ++ format isSolvable;
    if isSolvable {} {
        atLocation (0, 0) $
            prepareBoardAllRows (shouldCorrectTile $ mapTuple not evenOverlaps) boardWidth $ boardHeight - 1;
        return ()
    }
    end;

/**
Only about one in four randomly-assigned light patterns
are actual solvable lights-out games, so we make
an adjustment if our particular pattern is not solvable.

It so happens that an unsolvable board can be made
solvable by toggling exactly one carefully chosen light.
*/
def generateGame = \boardWidth. \boardHeight.

    prepareBoardRandom boardWidth boardHeight;

    evenOverlaps <- analyzeSolvability boardWidth boardHeight;
    ensureSolvability evenOverlaps boardWidth boardHeight;

    // Sanity checking:
    // evenOverlaps2 <- analyzeSolvability boardWidth boardHeight;
    // let isSolvable2 = fst evenOverlaps2 && snd evenOverlaps2 in
    // say $ "isSolvable2: " ++ format isSolvable2;

    // "Sentinel" to indicate that board preparation is complete
    create "flower";
    end;

def go =
    let boardWidth = 5 in
    let boardHeight = 5 in
    instant $ generateGame boardWidth boardHeight;
    observe;
    end;

go;