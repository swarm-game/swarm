def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

/**
Assumes we are on the left edge of a row of nucleobases
*/
def resetPlayfieldRow =
    dims <- floorplan "DNA decoder";
    let decoderWidth = fst dims in
    doN decoderWidth (grab; move;);
    end;

def resetPlayfield =
    teleport self (5, -2);
    resetPlayfieldRow;
    teleport self (5, -3);
    resetPlayfieldRow;

    teleport self (5, -11);
    resetPlayfieldRow;
    teleport self (5, -12);
    resetPlayfieldRow;
    end;

def watchSwitch = \lastState.
    watch down;
    wait 1000;
    found <- scan down;
    case found return (\item.
        if (item != lastState) {
            if (item == "switch (off)") {
                loc <- whereami;
                resetPlayfield;
                teleport self loc;
            } {
            };
        } {};
        watchSwitch item;
    );
    end;

def go =
    instant $ (
        found <- scan down;
        case found return watchSwitch;
    );
    end;

go;
