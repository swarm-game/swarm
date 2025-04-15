/**
Approach:
Place correct tiles in top row and left column,
then recurse into sub-rectangle.

NOTE: Much of this code is experimental and unused.
It may be revisited.

In particular, the Quad-tree search code should probably
be extracted into its own demo scenario.
*/

def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def signum = \x.
    if (x < 0) {-1}
    $ elif (x > 0) {1}
    $ else {0};
    end;

def mod : Int -> Int -> Int = \i.\m.
    i - m * (i / m);
    end

def abs = \n. if (n < 0) {-n} {n} end;

def λmatch = \f. \p. match p f end

def sumTuples = λmatch \t11. \t12. λmatch \t21. \t22.
    (t11 + t21, t12 + t22);
    end;

def mapTuple = \f. λmatch \a. \b. (f a, f b) end;

def getOrdinal : Text -> Cmd Int = \item.
    count $ item ++ "-ordinal";
    end;

/** One-based index
*/
def getLetterEntityByIndex = \idx.
    let letter = toChar $ idx - 1 + charAt 0 "a" in
    letter ++ "-tile";
    end;

def negateTuple = \t.
    mapTuple (\x. -x) t;
    end;

def subtractTuple = \t1. \t2.
    sumTuples t1 $ negateTuple t2;
    end;

def getRelativeLocation = \absLoc.
    myloc <- whereami;
    let negatedLoc = negateTuple myloc in
    pure $ sumTuples negatedLoc absLoc;
    end;

def getRelativeRectangle : (Int * Int) * (Int * Int) -> Cmd ((Int * Int) * (Int * Int)) = \corners.
    myloc <- whereami;
    let negatedLoc = negateTuple myloc in
    pure $ mapTuple (sumTuples negatedLoc) corners;
    end;

/**
Generates rectangle corners
relative to the player's current location,
given corners specified as absolute GPS coords.
*/
def getBoardRectangle =
    getRelativeRectangle ((0, 0), (3, -3));
    end;

/** Gets relative location to letter by its ordinal */
def getLetterLocation = \idx.
    corners <- getBoardRectangle;
    let entName = getLetterEntityByIndex idx in
    loc <- detect entName corners;
    end;

def moveTuple = λmatch \x. \y.
    turn $ if (x > 0) {east} {west};
    doN (abs x) move;

    turn $ if (y > 0) {north} {south};
    doN (abs y) move;
    end;

/**
Index is 1-based.
Coordinates are relative to the upper-left
corner of the board (which happens to be at (0,0) world coords),
with down being the negative Y direction.
*/
def getAbsoluteTargetLocationForIndex = \boardWidth. \n.
    let idx = n - 1 in
    (idx/boardWidth, -(mod idx boardWidth));
    end;

/**
The first Boolean argument indicates whether
we should extract the vertical or horizontal dimension.
*/
def getRectDimension = \vertical. \rect.
    let extractor = if vertical {λmatch \a. \_. a} {λmatch \_. \b. b} in
    match (mapTuple extractor rect) \a. \b.
    abs $ b - a;
    end;

def getOffsets = λmatch \a. \b. subtractTuple b a end;

def getAbsDelta = \rect.
    let difference = getOffsets rect in
    mapTuple abs difference;
    end;

def distance = \loc1. \loc2.
    let d = getAbsDelta (loc1, loc2) in
    match d \a. \b. a + b
    end;

// NOT USED
def selectCloser = \refloc. \loc1. \loc2.
    let d1 = distance refloc loc1 in
    let d2 = distance refloc loc2 in
    if (d1 < d2) {loc1} {loc2}
    end;

/**
Get rectangle dimensions as a tuple.

Since the rectangle is specified by "inclusive" corners,
we add one to each dimension of the absolute difference
of corners to obtain the corrected area.
*/
def getDimensions = \rect.
    let absDifference = getAbsDelta rect in
    mapTuple (\x. x + 1) absDifference;
    end;

def getRectArea = λmatch \w. \h. w * h end;

/**
Determines whether the vertical dimension is larger
than the horizontal dimension.
*/
def isVerticalLarger = λmatch \w. \h. h > w; end;

/**
Returns a tuple of the two rectangle partitions.

Precondition:
Rectangle corners are specified in the order:
  (top left, bottom right)

The "rightward/downward offset" (i.e. +1 horizontally, -1 vertically)
to the "avg" for the second rectangle depends on this assumption.
*/
def splitRectangle = \vertically. λmatch \cornerA. \cornerB.
    match cornerA \ax. \ay.
    match cornerB \bx. \by.
    if vertically {
        // TODO: We should "round down" the height of the second partition
        // so that the first partition is more likely to contain
        // the target (i.e. we perform one fewer iteration).
        let avgY = (ay + by) / 2 in
        let firstRect = (cornerA, (bx, avgY)) in
        let secondRect = ((ax, avgY - 1), cornerB) in
        (firstRect, secondRect)
    } {
        let avgX = (ax + bx) / 2 in
        let firstRect = (cornerA, (avgX, by)) in
        let secondRect = ((avgX + 1, ay), cornerB) in
        (firstRect, secondRect)
    };
    end;

def blankCellLocatorCriteria = \rect.
    entCount <- density rect;
    let dims = getDimensions rect in
    let tileCount = getRectArea dims in
    pure $ entCount < tileCount;
    end;

/**
Performs a quad-tree search for the blank square.

Partitioning can be either vertical or horizontal.
We always partition along the largest dimension.

The "first" partition in a vertical split is the "top" partition.
The "first" partition in a horizontal split is the "left" partition.
*/
def findEmptyCell = \foundCriteria. \rect.

    let dims = getDimensions rect in
    let tileCount = getRectArea dims in

    if (tileCount < 1) {
        pure $ inL ();
    } $ elif (tileCount == 1) {
        foundHere <- foundCriteria rect;
        pure $ if foundHere {
            match rect \w. \_. inR w;
        } {
            inL ();
        };
    } $ else {
        let isBiggerVertically = isVerticalLarger dims in
        let splitted = splitRectangle isBiggerVertically rect in
        match splitted \firstPartition. \secondPartition.

        foundInFirst <- foundCriteria firstPartition;
        let selectedPartition = if foundInFirst {firstPartition} {secondPartition} in
        findEmptyCell foundCriteria selectedPartition;
    };
    end;

/**
Out of two candidate locations, select the one
that is not co-linear with the blank tile location
and the letter location.

It is assumed that one of the candidates will meet
this criteria, so we only need check the first.
*/
def avoidCollinear = λmatch \blankx. \blanky. λmatch \tilex. \tiley. \loc1. \loc2.
    match loc1 \loc1x. \loc1y.
    let firstOk = if (blankx == tilex) {
            loc1x != tilex
        } $ elif (blanky == tiley) {
            loc1y != tiley
        } $ else {true} in
    if firstOk {loc1} {loc2};
    end;

/**
Move the blank to the cell that allows
the selected letter to advance in the
direction of its target location.

We select a cell with either a vertical offset
or a horizontal offset from the letter tile.
The offset must be in the direction of the
letter's ultimate destination.
*/
def getInitialBlankDestination = \blankLoc. \letterloc. \targetloc.
    let getCoord = \f. signum (f targetloc - f letterloc) + f letterloc in
    let fst = λmatch \a. \_. a in
    let snd = λmatch \_. \b. b in
    match letterloc \letterx. \lettery.
    avoidCollinear blankLoc letterloc (getCoord fst, lettery) (letterx, getCoord snd);
    end;

def moveSpaceToTile = \blankLoc. \targetRelativeLoc. \letterloc.

    log $ "first letter loc: " ++ format letterloc;

    // Note: this rectangle might not be "normalized" in terms
    // of corner ordering...
    let boundingPathRect = (letterloc, targetRelativeLoc) in
    log $ "Bounding rect: " ++ format boundingPathRect;

    let moveBlankTo = getInitialBlankDestination blankLoc letterloc targetRelativeLoc in
    log $ "Move blank to: " ++ format moveBlankTo;
    let incrementalBlankDest1 = getInitialBlankDestination letterloc blankLoc moveBlankTo in
    log $ "Incremental destination 1: " ++ format incrementalBlankDest1;

    moveTuple incrementalBlankDest1;
    end;

/**
Algorithm:
1. Bring the empty space into the rotational path of the letter.
2. Rotate the tile along the perimeter of the bounding rectangle.
*/
def placeTile = \boardWidth. \idx. \blankLoc.

    // TODO: Bring the empty space into the smallest
    // rectangle bounding both the letter and its
    // destination.

    log $ "empty space loc: " ++ format blankLoc;

    eitherLetterloc <- getLetterLocation idx;

    let targetLocAbsolute = getAbsoluteTargetLocationForIndex boardWidth idx in
    log $ "absolute target loc: " ++ format targetLocAbsolute;
    targetRelativeLoc <- getRelativeLocation targetLocAbsolute;

//    case eitherLetterloc pure $ moveSpaceToTile blankLoc targetRelativeLoc;
    moveTuple blankLoc;
    end;

def moveManually =
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    turn left;
    drill forward;
    move;
    drill forward;
    move;
    turn left;
    drill forward;

    move;
    turn left;
    drill forward;
    move;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    turn right;
    drill forward;
    move;
    drill forward;
    move;
    turn right;
    drill forward;
    end;

def go = \boardWidth.
    corners <- getBoardRectangle;
    eitherBlankLoc <- findEmptyCell blankCellLocatorCriteria corners;

    case eitherBlankLoc pure $ placeTile boardWidth 1;

    moveManually;
    end;

go 3;
