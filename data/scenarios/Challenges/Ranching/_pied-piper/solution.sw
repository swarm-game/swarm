def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;


def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def uTurn = \d.
    turn d;
    move;
    turn d;
    end;

/**
Starting at bottom right of field
*/
def harvestField = \fieldWidth.
    intersperse fieldWidth move harvest;
    uTurn right;
    intersperse fieldWidth move harvest;
    uTurn left;
    intersperse fieldWidth move harvest;
    uTurn right;
    intersperse fieldWidth move harvest;
    end;

def makeOatsCrumb = \spacing.
  place "oats";
  doN spacing move;
  end;

def waitUntilRatDisappeared =
  found <- scout east;
  if found {
      wait 1;
      waitUntilRatDisappeared;
  } {};
  end;

/**
Place a trail of breadcrumbs, and return
one crumb short of the first crumb.
*/
def makeOatsTrail = \spacing. \segments.
  doN segments $ makeOatsCrumb spacing;
  turn back;
  doN (segments * spacing) move;
  turn back;
  end;

def placeHorizontalTrail = \horizontalSpacing.
  turn east;
  doN (2*horizontalSpacing) move;
  place "oats";
  turn back;
  doN horizontalSpacing move;
  place "oats";
  doN horizontalSpacing move;
  turn left;
  end;

def waitForRatToPass =

  // Wait until the rat ate this crumb
  watch down;
  wait 2000;
  waitUntilRatDisappeared;
  end;

def makeTrails =
  let spacing = 4 in
  let segments = 5 in

  wait 30;
  placeHorizontalTrail 5;
  makeOatsTrail spacing $ segments + 1;
  end;

def getKey =
  turn east;
  doN 15 move;
  turn right;
  move;
  _k <- grab;
  turn right;
  doN 5 move;
  turn right;
  doN 6 move;
  turn left;
  doN 18 move;
  turn left;
  doN 20 move;
  turn right;
  doN 2 move;
  use "unlocker" forward;
  doN 6 move;
  turn right;
  doN 8 move;
  turn left;
  move;
  turn right;
  doN 6 move;
  turn left;
  move;
  end;

def placeMold =
  turn east;
  doN 11 move;
  sow "mold";
  end;

def go =
  getKey;
  harvestField 20;

  turn left;
  doN 2 move;
  turn left;
  harvestField 20;


  move;
  turn right;
  doN 14 move;
  turn left;

  // Get the mold
  doN 4 move;
  turn left;
  doN 3 move;
  harvest;
  turn back;
  doN 3 move;

  turn right;
  doN 3 move;
  turn left;

  // Head back to the house
  doN 8 move;
  turn left;
  doN 8 move;
  turn left;

  // Start laying trail
  intersperse 5 (doN 4 move) $ place "oats";
  placeHorizontalTrail 5;

  waitForRatToPass;

  makeTrails;
  waitForRatToPass;


  makeOatsTrail 4 10;
  placeHorizontalTrail 5;
  waitForRatToPass;

  makeOatsTrail 4 12;
  placeHorizontalTrail 5;
  waitForRatToPass;

  turn east;
  doN 2 move;

  placeHorizontalTrail 4;
  makeOatsTrail 4 4;
  waitForRatToPass;

  makeOatsTrail 4 6;
  placeHorizontalTrail 4;

  waitForRatToPass;
  makeOatsTrail 4 10;
  placeHorizontalTrail 4;


  waitForRatToPass;
  makeOatsTrail 4 12;
  placeHorizontalTrail 4;

  placeMold;
  end;

go;
