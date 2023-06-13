def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def sumTuples = \t1. \t2.
    (fst t1 + fst t2, snd t1 + snd t2);
    end;

def max = \a. \b.
    if (a > b) {a} {b};
    end;

def mapTuple = \f. \t.
    (f $ fst t, f $ snd t)
    end;

// modulus function (%)
def mod : int -> int -> int = \i.\m.
    i - m * (i / m)
end

def abs = \n. if (n < 0) {-n} {n} end

def isEven = \n.
    mod n 2 == 0;
    end

/*
Decide where to initially teleport to based on the initial coords.
*/
def init :
           [xMin : int, xMax : int, yMin : int, yMax : int]
        -> [yWest : int, yEast : int, xSouth : int, xNorth : int]
        -> cmd (bool * int) = \extents. \lanes.
    let topCorner = (-18, 30) in
    absloc <- whereami;
    let loc = sumTuples absloc $ mapTuple (\x. -x) topCorner in
    let xloc = abs $ fst loc in
    let idx = xloc / 2 in

    let yloc = abs (snd loc) in
    
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
    turn $ fst locdir;
    teleport self $ snd locdir;
    return (isLongitudinal, idx);
    end;

def isGreenLight = \isLongitudinal.
    r <- robotnamed "stoplight";
    isGreen <- as r {has "bit (1)"};
    return $ isLongitudinal != isGreen;
    end;

def getCanMove :
    [xWest : int, xEast : int, ySouth : int, yNorth : int]
    -> bool
    -> cmd bool
    = \stoplines. \hasGreenLight.

    d <- heading;
    loc <- whereami;
    let atStopLine = if (d == north) {
        snd loc == stoplines.yNorth;
    } $ elif (d == south) {
        snd loc == stoplines.ySouth;
    } $ elif (d == east) {
        fst loc == stoplines.xEast;
    } $ else {
        // west
        fst loc == stoplines.xWest;
    } in

    eitherNeighbor <- meet;
    // TODO: Make sure we only consider the neighbor directly in front of us.
    neighborIsStopped <- case eitherNeighbor
        (\_. return false)
        (\r. as r {has "bit (0)"}); // zero-bit means stopped

    return $ hasGreenLight || not (atStopLine || neighborIsStopped);
    end;

def doTunnelWrap : [xMin : int, xMax : int, yMin : int, yMax : int] -> cmd bool = \extents.
    myloc <- whereami;
    didWrap <- if (fst myloc < extents.xMin) {
        teleport self (extents.xMax, snd myloc);
        return true;
    } $ elif (fst myloc > extents.xMax) {
        teleport self (extents.xMin, snd myloc);
        return true;
    } $ elif (snd myloc < extents.yMin) {
        teleport self (fst myloc, extents.yMax);
        return true;
    } $ elif (snd myloc > extents.yMax) {
        teleport self (fst myloc, extents.yMin);
        return true;
    } $ else {
        return false;
    };
    return didWrap;
    end;

def moveWithWrap :
       [xWest : int, xEast : int, ySouth : int, yNorth : int]
    -> [xMin : int, xMax : int, yMin : int, yMax : int] // extents
    -> bool
    -> cmd (bool * bool)
    = \stoplines. \extents. \isLongitudinal.

    hasGreenLight <- isGreenLight isLongitudinal;
    canMove <- getCanMove stoplines hasGreenLight;

    wentThroughTunnel <- if canMove {
        move;
        doTunnelWrap extents;
    } {
        return false;
    };

    try {
        // Makes the "stopped" state queryable by other robots
        if canMove {make "bit (1)"} {make "bit (0)"}
    } {};

    return (canMove, wentThroughTunnel);
    end;

def getNewDelayState :
    bool
    -> [moveDelay : int, transitionCountdown : int]
    -> [moveDelay : int, transitionCountdown : int]
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
           int
        -> bool
        -> [xWest : int, xEast : int, ySouth : int, yNorth : int]
        -> [xMin : int, xMax : int, yMin : int, yMax : int]
        -> [moveDelay : int, transitionCountdown : int]
        -> cmd unit
        = \idx. \isLongitudinal. \stoplines. \extents. \delayState.

    wait delayState.moveDelay;

    result <- instant $ moveWithWrap stoplines extents isLongitudinal;
    let canGo = fst result in
    let wentThroughTunnel = snd result in
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
    result <- instant $ init extents lanes;
    let isLongitudinal = fst result in
    let idx = snd result in
    advance idx isLongitudinal stoplines extents [moveDelay=5, transitionCountdown=2];
    end;

go;