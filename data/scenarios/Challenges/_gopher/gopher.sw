def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def abs = \n. if (n < 0) {-n} {n} end;

def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def randSign = \x.
    opposite <- random 2;
    if (opposite == 1) {
        pure (-x);
    } {
        pure x;
    }
    end;

def goDir = \dist. \d1. \d2.
    if (dist < 0) {
        turn d1;
    } $ elif (dist > 0) {
        turn d2;
    } {};

    doN (abs dist) move;
    end;

def randSwap = \f. \g.
    r <- random 2;
    if (r == 0) {
        f; g;
    } {
        g; f;
    };
    end;

def converge = \dest. \currentLoc.
    let xDist = fst currentLoc - fst dest in
    let yDist = snd currentLoc - snd dest in
    randSwap (goDir xDist east west) (goDir yDist north south);
    end;

def navigateTo = \destTuple.
    loc <- whereami;
    converge destTuple loc;
    end;

def arrive = \fieldWidth. \fieldHeight.
    newDestinationX <- random fieldWidth;
    newDestinationYtemp <- random fieldHeight;
    let newDestinationY = -newDestinationYtemp in
    navigateTo (newDestinationX, newDestinationY);
    turn down;
    end;

def getTauntStage = \startingAmount. \newCount.
    if ((newCount * 5) / startingAmount < 1) {
        (0, "Hey, maybe we can work this out?")
    } $ elif ((newCount * 5) / startingAmount < 2) {
        (1, "I didn't hear no bell!")
    } $ elif ((newCount * 5) / startingAmount < 3) {
        (2, "Why don't you just give up?")
    } $ elif ((newCount * 5) / startingAmount < 4) {
        (3, "Close one!")
    } $ elif (newCount < startingAmount - 2) {
        (4, "OK, no more Mr. Nice Gopher!")
    } $ elif (newCount < startingAmount - 1) {
        (5, "Bet you can't do that again!")
    } $ elif (newCount < startingAmount) {
        (6, "Beginner's luck!")
    } $ else {(7, "You'll never catch me!")};
    end;

def waitWhileHere = \e. \remainingTime.
    here <- isHere e;
    if here {
        wait 1;
        if (remainingTime > 0) {
            waitWhileHere e $ remainingTime - 1;
        } {
            // Pick up the "dropping"
            try {
                // Note: There exists a race condition;
                // if the user happens to drill the mound
                // *after* the gopher checks it with `ishere`
                // but *before* it executes `grab`, the gopher will crash.
                // Thus we have wrapped this `grab` in a `try`.
                grab;
                pure ();
            } {};
        };
    } {};
    end;

def go = \width. \height. \lastTauntIndex. \startingAmount. \dropping.
    newCount <- count dropping;
    if (newCount > 0) {
        let tauntStage = getTauntStage startingAmount newCount in
        let tauntIndex = fst tauntStage in
        if (tauntIndex != lastTauntIndex) {
            say $ snd tauntStage;
        } {};

        appear "o" (inl ());
        arrive width height;

        place dropping;
        appear "G" (inl ());
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
            pure ();
        };

        baseloc <- as base {whereami};
        teleport self baseloc;
        give base "serenity";

        selfdestruct;
    };
    end;

let dropping = "mound" in
startingAmount <- count dropping;
go 28 18 (-1) startingAmount dropping;
