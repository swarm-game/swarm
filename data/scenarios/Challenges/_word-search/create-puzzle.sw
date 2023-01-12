def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def chooseLetter = \i.
    if (i == 0) {
        return "capital C";
    } {
        if (i == 1) {
            return "capital O";
        } {
            return "capital W";
        }
    };
    end;

def singleTile =

  letterIndex <- random 3;
  if (letterIndex == 2) {
    // We've selected the last letter in COW.
    //
    // TODO: To ensure there is only exactly one solution,
    // make sure we're not completing a word
    // horizontally (foward or backward) or
    // vertically (forward or backward), except if we
    // are in the pre-designated location.
    // 
    // To do this, we actually need to check for both
    // letterIndex 0 and 2. If we would be completing
    // a word, select a different random letter.
  } {};
  chosenLetter <- chooseLetter letterIndex;
  place chosenLetter;
  return ();
  end;

def crossBack = \n.
    turn right;
    move;
    turn right;
    doN (n - 1) move;
    turn back;
    end;

def layTiles = \n.
    intersperse n move singleTile;
    end;

def createPuzzle = \width. \height.
    intersperse height (crossBack width) (layTiles width);
    turn left;
    doN 4 move;
    turn right;
    move;

    // Remove the boulder blocking the player's path
    grab;
    end;

createPuzzle 25 15;