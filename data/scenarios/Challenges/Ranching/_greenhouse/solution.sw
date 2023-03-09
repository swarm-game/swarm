def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def intersperse = \n. \f2. \f1. if (n > 0) {
        f1;
        if (n > 1) {
            f2;
        } {};
        intersperse (n - 1) f2 f1;
    } {};
    end;

def ifC = \p.\t.\e.
  b <- p;
  if b t e
end;

def until = \p.\t.
  ifC p t {until p t}
end;

// Wait until the treads are here;
until (ishere "treads") {
    wait 30;
    t <- grab;
    equip t;
};

def isMushroom = \e.
  e == "truffle" || e == "portobello" || e == "chanterelle" || e == "shiitake";
  end;

// Returns the number of things grabbed.
def grabFirst =
  thingHere <- scan down;
  noGrab <- case thingHere (\_. return true) (\e.
    alreadyGot <- has e;
    let forbiddenItem = isMushroom e in
    return $ alreadyGot || forbiddenItem;
  );
  if noGrab {return 0;} {swap "decoy"; return 1};
  end;

def singleColumn = \height.
  intersperse height move grabFirst;
  end;

def turnaround = \d.
  intersperse 2 move $ turn d;
  end;

def twoColumns = \height.
  intersperse 2 (turnaround right) $ singleColumn height;
  end;

def harvestGarden = \height.
  let halfColumnCount = 20 in
  intersperse halfColumnCount (turnaround left) (twoColumns height);

  // We need to grab one last flower as a "souveneir"
  // to begin the planter box cycle.
  // NOTE: There is a very remote chance that we actually
  // needed to grab this corner flower already to obtain the last
  // flower variety, in which case this would fail...
  swap "decoy";
  end;

// Precondition: there exists
// nothing important in the
// current cell.
def plantSuccessorTo = \flower.
  swap flower;
  drillResult <- drill down;
  case drillResult return place;
  end;

/*
Take a sample of the previous flower
and "copy" into the next cell.
Then swap that planted cell with its
successor.
*/
def plantInGreenhouse = \flwr. \n.

  plantSuccessorTo flwr;
  flower <- harvest;
  if (n > 1) {
    move;
    plantInGreenhouse flower $ n - 1;
  } {
    return flower;
  };
  end;

def go =
  doN 4 move;

  souveneir <- harvestGarden 8;

  let pouch = "flower pouch" in
  make pouch;
  equip pouch;

  // Go down to the planter boxes in the greenhouse
  doN 8 move;
  turn right;
  doN 7 move;

  lastFlower <- plantInGreenhouse souveneir 20;
  turn left;
  doN 2 move;
  turn left;
  doN 3 move;

  _lastFlower2 <- plantInGreenhouse lastFlower 20;

  // Get out of the way so we can see the last flower
  doN 2 move;
  end;

go;