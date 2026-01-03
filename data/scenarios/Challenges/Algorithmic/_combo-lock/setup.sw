import "~swarm/lib/control"

def colorFromIndex = \i.
    if (i == 0) {"R"}
    $ elif (i == 1) {"G"}
    $ else {"B"};
    end;

def pixelFromColor = \c.
    "dial (" ++ c ++ ")";
    end;

def checkCombo = \noMismatchYet. \stepsTaken. \colorString.

    let remainingCount = chars colorString in
    if (remainingCount > 0) {

        let splitted = split 1 colorString in
        match splitted \nextLetter. \remainingLetters.
        let expectedPixel = pixelFromColor nextLetter in

        move;
        isExpectedHere <- ishere expectedPixel;
        checkCombo (isExpectedHere && noMismatchYet) (stepsTaken + 1) remainingLetters;
    } {
        turn back;

        // Replace the cell watches
        doN stepsTaken (watch down; move);
        turn back;
        pure noMismatchYet;
    };
    end;

def unlockGate = \n.
    move;
    turn right;
    move;
    turn left;
    doN n (grab; move);
    pure ()
    end;

def doUntilCorrect = \colorString.
    isCorrect <- checkCombo true 0 colorString;
    if isCorrect {
        let remainingCount = chars colorString in
        unlockGate remainingCount;
        pure true;
    } {
        wait 1000;
        doUntilCorrect colorString;
    };
    end;

def createCombo = \colorString.
    // Scenario map starts with red pixels to
    // mark the combo sequence
    redPixelHere <- ishere $ pixelFromColor "R";

    if redPixelHere {
        r <- random 3;
        let newColor = colorFromIndex r in
        watch down;
        move;
        createCombo $ newColor ++ colorString;
    } {
        turn back;
        pure colorString;
    };
    end;

def go =
    comboString <- instant {
        move;
        createCombo "";
    };
    // say comboString;
    instant {doUntilCorrect comboString};
    end;
