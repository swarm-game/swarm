def forever : cmd unit -> cmd unit = \c. c ; forever c end

/** Teleports to a new location to execute a function
  then returns to the original location before
  returning the function's output value.
*/
def atLocation = \newLoc. \f.
    prevLoc <- whereami;
    teleport self newLoc;
    retval <- f;
    teleport self prevLoc;
    return retval;
    end;

def swapItem = \ent.
  create ent;
  emptyHere <- isempty;
  if emptyHere {} {grab; return ()};
  place ent;
  end;

def setRedPixel =
  instant $ (
    swapItem "pixel (R)";
  );
  end;

def setGreenPixel =
  instant $ (
    swapItem "pixel (G)";
  );
  end;

def changeToRed =
  say "Red light";
  make "bit (0)";
  setRedPixel;
  atLocation (17, 2) setGreenPixel;
  wait 50;
  end;

def changeToGreen =
  say "Green light";
  make "bit (1)";
  setGreenPixel;
  atLocation (17, 2) setRedPixel;
  wait 100;
  end;

def alternate =
  changeToGreen;
  changeToRed;
  end;

forever alternate;