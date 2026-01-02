import "../../../../lib/control"
import "../../../../lib/list"
import "../../../../lib/arith"

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
Repeatedly generate a random number until
we find one that's not in the bitmask.
*/
def getUnusedRandom = \maxval. \bitmask.
    nextRandomVal <- random maxval;
    if (isBitSet bitmask nextRandomVal) {
        getUnusedRandom maxval bitmask;
    } {
        pure nextRandomVal;
    }
    end;

/**
Use the `random` function to generate a random permuation of `n` contiguous values.
Uses a bitmask to ensure uniqueness.

Fisher-Yates would be more efficient, but requires a physical array.
*/
def naiveRandomStack = \valueFunc. \maxval. \bitmask. \n.
    val <- if (n > 1) {
        nextRandomVal <- getUnusedRandom maxval bitmask;
        let newBitmask = bitmask + shiftLeft 1 nextRandomVal in
        naiveRandomStack valueFunc maxval newBitmask $ n - 1;
        pure nextRandomVal;
    } {
        // We're at the peak of the stack.
        // Now we unwind it.

        // Saves some time in generating the last number by inferring the
        // only remaining possible choice.
        let missingBit = getMissingBit bitmask maxval in
        pure missingBit;
    };
    valueFunc val;
    end;

def busts : (rec l. Unit + Text * l) = tagmembers "bust" end
def bustCount : Int = length busts end

def placeThing = \entIdx.
    let entName = index entIdx busts in
    create entName;
    place entName;
    end;

def placeEntByIndex = \bitIndex.
    placeThing bitIndex;
    move;
    end;

/*
Generates a constant-time lookup table from
name to ordinal for both the judge robot
and the base.

The "baseExtraCount" gets carried over
across iterations to ensure the count for each
bust in the base's inventory increases monotonically.

"idx" counts upwards.
*/
def populateInventory = \baseCount. \idx.

    if (idx < bustCount) {

        let item = index idx busts in

        // Give copies to the base
        baseExtras <- random 5;
        let newBaseCount = 1 + baseCount + baseExtras in
        doN newBaseCount (
            create item;
            give base item;
        );

        // Keep copies for myself
        doN (idx + 1) (
            create item;
        );

        populateInventory newBaseCount $ idx + 1;
    } {};

    end;

def setup =
    populateInventory 0 0;

    naiveRandomStack placeEntByIndex bustCount 0 bustCount;
    turn back;
    move;
    create "bitcoin";
    end;
