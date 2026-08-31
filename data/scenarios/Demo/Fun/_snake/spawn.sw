import "~swarm/lib/scan"

def waitForConsumption =
  watch down;
  wait 2000;
  here <- scan down;
  if (here == "apple") {
    waitForConsumption;
  } {}
  end;

// TODO Ensure we don't spawn inside a coil of the tail
def placeAtOpenLocation = \range.
  randX <- random range;
  randY <- random range;
  let x = range/2 - randX in
  let y = range/2 - randY in
  teleport self (x, y);
  emptyHere <- isempty;
  if emptyHere {
    place "apple";
  } {
    placeAtOpenLocation range;
  }
  end;

def repeatedlyPlaceApple = \range.
  placeAtOpenLocation range;
  waitForConsumption;
  end;

def go =
  instant {repeatedlyPlaceApple 20};
  go;
  end;
