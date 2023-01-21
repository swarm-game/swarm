def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def makeSigned = \b. \x.
    if b {
        return (-x);
    } {
        return x;
    }
    end;

def getDirection = \n.
    if (n == 0) {
        return forward;
    } {
        if (n == 1) {
            return right;
        } {
            if (n == 2) {
                return back;
            } {
                if (n == 3) {
                    return left;
                } {
                    return down;
                }
            }
        }
    }
    end;

/**
Loops forever
*/
def scanDirections = \n.
    d <- getDirection n;
    out <- scan d;
    shouldContinue <- case out
        (\_. return true)
        (\x. if (x == "mound") {
            drill d;
            return true;
        } {
            // A "flower" shall serve as
            // a semaphore to terminate the loop,
            // so that the base can `salvage` us.
            return $ x != "flower";
        });

    if shouldContinue {
        if (n > 0) {
            scanDirections $ n - 1;
        } {
            // Inserting this "wait" speeds up the
            // integration test significantly (more than 3x)
            wait 64;
            scanDirections 4;
        };
    } {};
    end;

def deploySensor =
    _s <- build {scanDirections 0;};
    return ();
    end;

def isDivisibleBy = \dividend. \divisor.
    (dividend / divisor) * divisor == dividend;
    end;

// stagger at every fifth cell
def deployRow = \offset. \cellCount.
    if (isDivisibleBy (cellCount + offset) 5) {
        deploySensor;
    } {};
    if (cellCount > 1) {
        move;
        deployRow offset $ cellCount - 1;
    } {};
    end;

def isEven = \x.
   isDivisibleBy x 2
   end;

def deployGrid = \width. \height.

    if (height > 0) {

        let nowEven = isEven height in

        offsetVal <- makeSigned nowEven $ height * 2;
        extraOffset <- if nowEven {
            return (-1);
        } {
            return 0;
        };
        deployRow (offsetVal + extraOffset) width;

        d <- if nowEven {
            return right;
        } {
            return left;
        };

        turn d;
        move;
        turn d;

        deployGrid width $ height - 1;
    } {};
    end;

def pickupToolkit =
    x <- scan down;
    case x
        (\_. return false)
        (\y. if (y == "toolkit") {
            grab; return true
        } {
            return false
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
        d <- if nowEven {
            return right;
        } {
            return left;
        };

        turn d;
        move;
        turn d;

        searchToolkitRows width $ rowCount - 1;
    } {};
    end;

def listenForDefeat =
    // Block until the gopher says something
    m <- listen;
    if (m == "Argh! I give up.") {} {
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
    deployGrid width height;
    turn left;
    move;

    listenForDefeat;
    searchForToolkit height width;
    end;

go 30 20;