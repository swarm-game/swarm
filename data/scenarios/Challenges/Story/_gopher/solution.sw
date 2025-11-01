def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def makeSigned = \b. \x.
    if b {-x} {x};
    end;

def getDirection = \n.
    if (n == 0) {
        forward;
    } $ elif (n == 1) {
        right;
    } $ elif (n == 2) {
        back;
    } $ elif (n == 3) {
        left;
    } $ else {
        down;
    };
    end;

def watchDir = \n.
    watch $ getDirection n;
    if (n > 0) {
        watchDir $ n - 1;
    } {};
    end;

/**
Loops forever
*/
def scanDirections = \n.
    let d = getDirection n in
    out <- scan d;
    shouldContinue <- case out
        (\_. pure true)
        (\x. if (x == "mound") {
            drill d;
            pure true;
        } {
            // A "flower" shall serve as
            // a semaphore to terminate the loop,
            // so that the base can `salvage` us.
            pure $ x != "flower";
        });

    if shouldContinue {
        if (n > 0) {
            scanDirections $ n - 1;
        } {
            watchDir 4;
            wait 1000;
            scanDirections 4;
        };
    } {};
    end;

def deploySensor =
    _s <- build {scanDirections 0};
    pure ();
    end;

def isDivisibleBy = \dividend. \divisor.
    (dividend / divisor) * divisor == dividend;
    end;

// stagger at every fifth cell
def deployRow = \f. \offset. \cellCount.
    if (isDivisibleBy (cellCount + offset) 5) {
        f;
    } {};
    if (cellCount > 1) {
        move;
        deployRow f offset $ cellCount - 1;
    } {};
    end;

def isEven = \x.
   isDivisibleBy x 2
   end;

def deployGrid = \f. \width. \height.

    if (height > 0) {

        let nowEven = isEven height in
        let offsetVal = makeSigned nowEven $ height * 2 in
        let extraOffset = if nowEven {-1} {0} in
        deployRow f (offsetVal + extraOffset) width;

        let d = if nowEven {right} {left} in
        turn d;
        move;
        turn d;
        deployGrid f width $ height - 1;
    } {};
    end;

def pickupToolkit =
    x <- scan down;
    case x
        (\_. pure false)
        (\y. if (y == "toolkit") {
            tk <- grab;
            equip tk;
            pure true
        } {
            pure false
        });
    end;

def searchToolkitSingleRow = \colCount.

    _found <- pickupToolkit;
    if (colCount > 1) {
        move;
        searchToolkitSingleRow $ colCount - 1;
    } {};
    end;    

def searchToolkitRows = \width. \rowCount.
    searchToolkitSingleRow width;
    if (rowCount > 1) {

        let nowEven = isEven rowCount in
        let d = if nowEven {right} {left} in

        turn d;
        move;
        turn d;

        searchToolkitRows width $ rowCount - 1;
    } {};
    end;

def listenForDefeat =
    // Block until the gopher is defeated
    m <- has "serenity";
    if m {} {
        listenForDefeat;
    };
    end;

def searchForToolkit = \width. \height.
    searchToolkitRows width height;

    turn right;
    doN (height - 1) move;
    turn right;
    doN (width - 1) move;
    turn right;
    end;

def go = \width. \height.
    deployGrid deploySensor width height;
    turn left;
    move;

    listenForDefeat;
    searchForToolkit height width;

    deployGrid (place "flower") width height;
    turn left;
    doN height move;
    turn right;
    deployGrid salvage width height;
    end;

go 30 20;