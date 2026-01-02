import "../../../lib/control"
import "../../../lib/tuple"
import "../../../lib/arith"

/*
Decide where to initially teleport to based on the initial coords.
*/
def init :
           [xMin : Int, xMax : Int, yMin : Int, yMax : Int]
        -> [yWest : Int, yEast : Int, xSouth : Int, xNorth : Int]
        -> Cmd (Bool * Int) = \extents. \lanes.
    let topCorner = (-18, 30) in
    absloc <- whereami;
    let loc = sumTuples absloc $ mapTuple (\x. -x) topCorner in
    match loc \x. \y.
    let xloc = abs x in
    let idx = xloc / 2 in
    let yloc = abs y in

    randoffset <- random 5;
    let baseoffset = 10 * idx in
    let offset = randoffset + baseoffset in
    let isLongitudinal = not $ isEven yloc in
    let locdir = if isLongitudinal {
        if (isEven xloc) {
            (south, (lanes.xSouth, extents.yMax - offset))
        } {
            (north, (lanes.xNorth, extents.yMin + offset))
        }
    } {
        if (isEven xloc) {
            (east, (extents.xMin + offset, lanes.yEast))
        } {
            (west, (extents.xMax - offset, lanes.yWest))
        }
    } in
    match locdir \dir. \loc.
    turn dir;
    teleport self loc;
    pure (isLongitudinal, idx);
    end;

def isGreenLight = \isLongitudinal.
    r <- robotnamed "stoplight";
    isGreen <- as r {has "bit (1)"};
    pure $ isLongitudinal != isGreen;
    end;

def getCanMove :
    [xWest : Int, xEast : Int, ySouth : Int, yNorth : Int]
    -> Bool
    -> Cmd Bool
    = \stoplines. \hasGreenLight.

    d <- heading;
    loc <- whereami;
    match loc \x. \y.
    let atStopLine = if (d == north) {
        y == stoplines.yNorth;
    } $ elif (d == south) {
        y == stoplines.ySouth;
    } $ elif (d == east) {
        x == stoplines.xEast;
    } $ else {
        // west
        x == stoplines.xWest;
    } in

    eitherNeighbor <- meet;
    // TODO: Make sure we only consider the neighbor directly in front of us.
    neighborIsStopped <- case eitherNeighbor
        (\_. pure false)
        (\r. as r {has "bit (0)"}); // zero-bit means stopped

    pure $ hasGreenLight || not (atStopLine || neighborIsStopped);
    end;

def doTunnelWrap : [xMin : Int, xMax : Int, yMin : Int, yMax : Int] -> Cmd Bool = \extents.
    myloc <- whereami;
    match myloc \myx. \myy.
    didWrap <- if (myx < extents.xMin) {
        teleport self (extents.xMax, myy);
        pure true;
    } $ elif (myx > extents.xMax) {
        teleport self (extents.xMin, myy);
        pure true;
    } $ elif (myy < extents.yMin) {
        teleport self (myx, extents.yMax);
        pure true;
    } $ elif (myy > extents.yMax) {
        teleport self (myx, extents.yMin);
        pure true;
    } $ else {
        pure false;
    };
    pure didWrap;
    end;

def moveWithWrap :
       [xWest : Int, xEast : Int, ySouth : Int, yNorth : Int]
    -> [xMin : Int, xMax : Int, yMin : Int, yMax : Int] // extents
    -> Bool
    -> Cmd (Bool * Bool)
    = \stoplines. \extents. \isLongitudinal.

    hasGreenLight <- isGreenLight isLongitudinal;
    canMove <- getCanMove stoplines hasGreenLight;

    wentThroughTunnel <- if canMove {
        move;
        doTunnelWrap extents;
    } {
        pure false;
    };

    try {
        // Makes the "stopped" state queryable by other robots
        if canMove {make "bit (1)"} {make "bit (0)"}
    } {};

    pure (canMove, wentThroughTunnel);
    end;

def getNewDelayState :
    Bool
    -> [moveDelay : Int, transitionCountdown : Int]
    -> [moveDelay : Int, transitionCountdown : Int]
    = \canGo. \delayState.
    if (not canGo) {
        // reset to max delay and pause the countdown at max
        [moveDelay=5, transitionCountdown=2];
    } $ elif (delayState.moveDelay <= 0) {
        // unchanged
        delayState
    } $ elif (delayState.transitionCountdown > 0) {
        // decrement countdown
        [moveDelay=delayState.moveDelay, transitionCountdown=delayState.transitionCountdown - 1];
    } $ else {
        // Decrement the delay and reset the countdown.
        [moveDelay=max 0 $ delayState.moveDelay - 1, transitionCountdown=2];
    }
    end;

/**
Initially we wait several ticks between movements.
Then we continually decrease the delay by 1, until reaching no delay.
*/
def advance :
           Int
        -> Bool
        -> [xWest : Int, xEast : Int, ySouth : Int, yNorth : Int]
        -> [xMin : Int, xMax : Int, yMin : Int, yMax : Int]
        -> [moveDelay : Int, transitionCountdown : Int]
        -> Cmd Unit
        = \idx. \isLongitudinal. \stoplines. \extents. \delayState.

    wait delayState.moveDelay;

    result <- instant {moveWithWrap stoplines extents isLongitudinal};
    match result \canGo. \wentThroughTunnel.
    if wentThroughTunnel {
        r <- random 50;
        wait $ idx * 10 + r;
    } {};

    let newDelay = getNewDelayState canGo delayState in
    advance idx isLongitudinal stoplines extents newDelay;
    end;

def go =
    let extents = [xMin = -12, xMax=53, yMin = -15, yMax = 26] in
    let lanes = [yWest = 7, yEast = 5, xSouth = 20, xNorth = 22] in
    let stoplines = [xWest = 24, xEast = 17, ySouth = 9, yNorth = 2] in
    result <- instant {init extents lanes};
    match result \isLongitudinal. \idx.
    advance idx isLongitudinal stoplines extents [moveDelay=5, transitionCountdown=2];
    end;
