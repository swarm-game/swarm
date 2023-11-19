def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

// modulus function (%)
def mod : int -> int -> int = \i. \m.
  i - m * (i / m)
end

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def until = \p. \c. q <- p; if q {} {c; until p c} end;
def while = \p. until (x <- p; return $ not x) end;

def isDivisibleBy = \dividend. \divisor.
    (dividend / divisor) * divisor == dividend;
    end;

def isEven = \x.
   isDivisibleBy x 2
   end;

/**
Performs a right bitshift of "x" by "n" places
*/
def shiftRight = \x. \n.
    x / (2^n);
    end;

/**
Performs a left bitshift of "x" by "n" places
*/
def shiftLeft = \x. \n.
    x * (2^n);
    end;

/**
Checks whether the bit at index "idx" is set in the "bitmask".
zero-based indexing; 0 is the LSB.
*/
def isBitSet = \bitmask. \idx.
    not $ isEven $ shiftRight bitmask idx;
    end;

/**
Tests whether only a single bit is set in the bitmask.
Aborts early with 'false' if a second bit is detected.
*/
def exactlyOneBit = \foundOneBit. \bitmask.
    if (bitmask == 0) {
      foundOneBit;
    } {
      let bitIsSet = not $ isEven bitmask in
      if (foundOneBit && bitIsSet) {
        false;
      } {
        exactlyOneBit (foundOneBit || bitIsSet) $ bitmask / 2;
      }
    }
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

def placeSand =
    let item = "sand" in
    create item;
    place item;
    move;
    end;

/**
Repeatedly generate a random number until
we find one that's not in the bitmask.
*/
def getUnusedRandom = \maxval. \bitmask.
    nextRandomVal <- random maxval;
    if (isBitSet bitmask nextRandomVal) {
        getUnusedRandom maxval bitmask;
    } {
        return nextRandomVal;
    }
    end;

def getEntName = \idx.
    if (idx == 1) {
        "grape"
    } $ elif (idx == 2) {
        "lemon"
    } $ elif (idx == 3) {
        "apple"
    } $ elif (idx == 4) {
        "blueberry"
    } $ elif (idx == 5) {
        "watermelon"
    } $ elif (idx == 6) {
        "orange"
    } $ else {
        "dragonfruit"
    }
    end;

def getMissingBitRecursive = \bitmask. \idx.
    if (idx > 0) {
        if (isEven bitmask) {
            idx
        } {
            getMissingBitRecursive (bitmask / 2) $ idx - 1;
        }
    } {
        // The MSB was the missing bit.
        0;
    }
    end;

/**
Returns the index of the right-most bit that is zero.
*/
def getMissingBit = \bitmask. \maxIdx.
    let val = getMissingBitRecursive bitmask maxIdx in
    maxIdx - val;
    end;

/**
Use the `random` function to generate a random permuation of `n` contiguous values.
Uses a bitmask to ensure uniqueness.

Fisher-Yates would be more efficient, but requires a physical array.
*/
def naiveRandomStack = \valueFunc. \maxval. \bitmask. \n.
    val <- if (n > 1) {
        nextRandomVal <- getUnusedRandom maxval bitmask;

        // Recursion bug workaround (see #1032):
        let blahNextRandomVal = nextRandomVal in

        let newBitmask = bitmask + shiftLeft 1 nextRandomVal in
        naiveRandomStack valueFunc maxval newBitmask $ n - 1;
        return blahNextRandomVal;
    } {
        // We're at the peak of the stack.
        // Now we unwind it.

        // Saves some time in generating the last number by inferring the
        // only remaining possible choice.
        let missingBit = getMissingBit bitmask maxval in
        return missingBit;
    };
    valueFunc val;
    end;

def placeThing = \entIdx.
    let entName = getEntName entIdx in
    create entName;
    place entName;
    end;

def placeEntsForBits = \bitmask. \bitIndex.
    if (isBitSet bitmask bitIndex) {
        placeThing bitIndex;
        move;
    } {};
    end;

def columnFunc = \exclusionValue. \inputCardinality. \x.
    if (x != 0 && x != exclusionValue && not (exactlyOneBit false x)) {
        naiveRandomStack (placeEntsForBits x) inputCardinality 0 inputCardinality;
        myloc <- whereami;
        teleport self (fst myloc + 1, 0);
    } {};
    end;

def makeSandRow = \length.
    turn east;
    atLocation (0, -1) $ doN length placeSand;
    end;

def chooseExclusionValue = \powersetCardinality.

    // For cardinality 32, for example, the value of "r"
    // will be between 0 and 30, inclusive.
    r <- random $ powersetCardinality - 1;

    // We offset by one so as not to exclude zero.
    // So the exclusion value is now between
    // 1 and 31, inclusive.
    let value = r + 1 in

    if (exactlyOneBit false value) {
      chooseExclusionValue powersetCardinality;
    } {
      return value;
    }
    end;

/**
"inputCardinality" is the number of distinct entities
*/
def setup = \inputCardinality.
    let powersetCardinality = 2^inputCardinality in
    makeSandRow $ powersetCardinality - (1 + inputCardinality);

    turn north;
    move;
    exclusionValue <- chooseExclusionValue powersetCardinality;
    naiveRandomStack (columnFunc exclusionValue inputCardinality) powersetCardinality 0 powersetCardinality;
    return exclusionValue;
    end;

/**
One-based ordinal of the item.
*/
def getOrdinal : text -> cmd int = \item.
    count item;
    end;

def checkSolutionSum = \runningSum.
    maybeItem <- scan down;
    case maybeItem (\_. return runningSum) (\item.
        // The bell is the only other item we can place in this
        // scenario besides the fruits.
        if (item != "bell") {
            theOrdinal <- getOrdinal item;
            let binaryValue = shiftLeft 1 $ theOrdinal - 1 in
            move;
            checkSolutionSum $ binaryValue + runningSum;
        } {return runningSum};
    );
    end;

def waitForFirstPlacement =
    watch down;
    wait 1000;
    emptyhere <- isempty;
    if emptyhere {waitForFirstPlacement} {};
    end;

def go = \distinctCount.
    exclusionValue <- instant $ setup distinctCount;
    give base "bell";

    waitForFirstPlacement;
    while (as base {has "bell"}) $ wait 2;
    theSum <- checkSolutionSum 0;
    let sentinelItem = if (exclusionValue == theSum) {"bit (1)"} {"bit (0)"} in
    create sentinelItem;
    end;

go 7;