/**
Uses a string to maintain a queue of coordinates.
*/

def coordsToString : (int * int) -> text = \coords.
  format (fst coords) ++ "," ++ format (snd coords)
  end

def indexOfRec : int -> text -> text -> (unit + int) = \pos. \inputString. \targetChar.
  if (pos >= chars inputString) {
    inL ()
  } {
    if (toChar (charAt pos inputString) == targetChar) {
      inR pos
    } {
      indexOfRec (pos + 1) inputString targetChar
    }
  }
  end

def indexOf : text -> text -> (unit + int) =
  indexOfRec 0
  end

// Drops the first character of a string
def strTail : text -> text = \inputString.
  snd $ split 1 inputString
  end

def splitOnFirstChar : text -> text -> (text * text) = \inputString. \splitChar.
  case (indexOf inputString splitChar) (\_.
    // Did not find the split character, so return the original string
    (inputString, "")
  ) (\foundIdx.
    let parts = split foundIdx inputString in
    (fst parts, strTail $ snd parts)
  )
  end

def getDecimalCharValue = \inputString. \idx.
  charAt idx inputString - charAt 0 "0"
  end

// Works from right to left
def parseDecimalRec : int -> text -> int = \charsRemaining. \inputString.
  if (charsRemaining > 0) {
    getDecimalCharValue inputString (charsRemaining - 1) + 10 * parseDecimalRec (charsRemaining - 1) inputString
  } {0}
  end

def parseDecimal : text -> int = \inputString.
  let isNegative = toChar (charAt 0 inputString) == "-" in
  let negationMultiplier = if isNegative {-1} {1} in
  let modifiedString = if isNegative {strTail inputString} {inputString} in
  let stringLength = chars modifiedString in
  negationMultiplier * parseDecimalRec stringLength modifiedString;
  end

// Comma (",") is the separator between abscissa and ordinate
def stringToCoords : text -> (int * int) = \coordsString.
  let pair = splitOnFirstChar coordsString "," in
  (parseDecimal $ fst pair, parseDecimal $ snd pair)
  end

// APPEND to string representation of a coordinate list
def snoc : (int * int) -> text -> text = \coords. \strList.
  let delimiter = if (chars strList > 0) {";"} {""} in
  strList ++ delimiter ++ coordsToString coords;
  end

// Extracts the first element and returns the shortened list
def pop : text -> (unit + ((int * int) * text)) = \strList.
  if (chars strList > 0) {
    let pair = splitOnFirstChar strList ";" in
    inR (stringToCoords $ fst pair, snd pair)
  } {
    inL ();
  }
  end

def getDir = \dest.
    path (inL ()) (inL dest);
    end;

def doAtLoc = \currLoc. \targetLoc. \func.
  teleport self targetLoc;
  x <- func;
  teleport self currLoc;
  return x;
  end;

def moveTail = \tailList.
  emptyHere <- isempty;
  if emptyHere {
    let maybeShifted = pop tailList in
    case maybeShifted (\_.
      // Nothing to pick up or replace
      return tailList;

    ) (\newPair.

        let farthestTail = fst newPair in
        let newInit = snd newPair in
        newLoc <- whereami;
        grabbedItem <- doAtLoc newLoc farthestTail grab;
        place grabbedItem;

        return $ snoc newLoc newInit;
    );
  } {
    return tailList;
  }
  end;

def moveOneStep = \tailList.

    // This robot will always be sitting atop the apple
    r <- robotnamed "spawn";
    targetLoc <- as r {whereami};

    maybeD <- getDir targetLoc;
    case maybeD (\_. say "Dead!"; return "") (\d.
      turn $ fst d;
      newList <- moveTail tailList;
      move;
      return newList
    );
    end

// Invariant: No tail pieces shall ever be moved underneath the
// snake robot, unless an apple was just picked up.
def moveToApple = \tailList.

  myLoc <- whereami;

  appleHere <- ishere "apple";
  if appleHere {

    modifiedTailList <- try {
      make "tail";
      swap "tail";
      return $ snoc myLoc tailList;
    } {
      grab;
      return tailList;
    };
    // Need to move here so that we get out of the way
    // if the tail gets elongated
    moveOneStep modifiedTailList;
  } {
    moveOneStep tailList;
  }
  end;

def go = \tailList.
  newList <- instant $ moveToApple tailList;
  go newList;
  end;

go "";
