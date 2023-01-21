def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def abs = \n. if (n < 0) {-n} {n} end;

def randSign = \x.
    opposite <- random 2;
    if (opposite == 1) {
        return (-x);
    } {
        return x;
    }
    end;

def converge = \dest. \currentLoc.

  let xDist = fst currentLoc - fst dest in
  let yDist = snd currentLoc - snd dest in

  if (xDist < 0) {
    turn east;
  } {
    if (xDist > 0) {
      turn west;
    } {};
  };
  doN (abs xDist) move;

  if (yDist < 0) {
    turn north;
  } {
    if (yDist > 0) {
      turn south;
    } {};
  };
  doN (abs yDist) move;
  end;

/**
TODO: Randomly alternate between horizontal and vertical
movement.
*/
def navigateTo = \destTuple.
    loc <- whereami;
    converge destTuple loc;
    end;

def arrive = \fieldWidth. \fieldHeight.

    let leadDist = 20 in

    newDestinationX <- random fieldWidth;
    newDestinationYtemp <- random fieldHeight;
    let newDestinationY = -newDestinationYtemp in

    offsetXrand <- random $ leadDist / 2;

    // The manhattan-distance of the offset
    // must total some preset amount.
    let offsetXunsigned = (leadDist / 2) + offsetXrand in
    let offsetYunsigned = leadDist - offsetXunsigned in

    offsetX <- randSign offsetXunsigned;
    offsetY <- randSign offsetYunsigned;

    let startX = newDestinationX + offsetX in
    let startY = newDestinationY + offsetY in

    teleport self (startX, startY);
    navigateTo (newDestinationX, newDestinationY);
    turn down;
    end;

def getTauntStage = \startingAmount. \newCount.
    if ((newCount * 5) / startingAmount < 1) {
        return (0, "Hey, maybe we can work this out?");
    } {
        if ((newCount * 5) / startingAmount < 2) {
            return (1, "I didn't hear no bell!");
        } {
            if ((newCount * 5) / startingAmount < 3) {
                return (2, "Why don't you just give up?");
            } {
                if ((newCount * 5) / startingAmount < 4) {
                    return (3, "Close one!");
                } {
                    if (newCount < startingAmount - 2) {
                        return (4, "OK, no more Mr. Nice Gopher!");
                    } {
                        if (newCount < startingAmount - 1) {
                            return (5, "Bet you can't do that again!");
                        } {
                            if (newCount < startingAmount) {
                                return (6, "Beginner's luck!");
                            } {
                                return (7, "You'll never catch me!");
                            };
                        };
                    };
                };
            };
        };
    };
    end;

def waitWhileHere = \e. \remainingTime.
    here <- isHere e;
    if here {
        wait 1;
        if (remainingTime > 0) {
            waitWhileHere e $ remainingTime - 1;
        } {
            // Pick up the "dropping"
            grab;
            return ();
        };
    } {};
    end;

def go = \width. \height. \lastTauntIndex. \startingAmount. \dropping.
    newCount <- count dropping;
    if (newCount > 0) {
        tauntStage <- getTauntStage startingAmount newCount;
        let tauntIndex = fst tauntStage in
        if (tauntIndex != lastTauntIndex) {
            say $ snd tauntStage;
        } {};

        appear "o";
        arrive width height;

        place dropping;
        appear "G";
        waitWhileHere dropping 150;
        go width height tauntIndex startingAmount dropping;
    } {
        say "Argh! I give up.";

        // Allow the player to salvage their robots
        let reward = "toolkit" in
        try {
            place reward;
        } {
            swap reward;
            return ();
        };

        selfdestruct;
    };
    end;

let dropping = "mound" in
startingAmount <- count dropping;
go 28 18 (-1) startingAmount dropping;