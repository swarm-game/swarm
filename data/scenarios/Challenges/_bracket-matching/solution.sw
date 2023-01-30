def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

/**
Open = True
Close = False
*/
def bracketForBool = \item.
    if (item == "left bracket") {
        return $ inr true;
    } {
        if (item == "right bracket") {
            return $ inr false;
        } {
            return $ inl ();
        };
    };
    end;

def bitFromBool = \b.
    if b {
        return "bit (1)";
    } {
        return "bit (0)";
    }
    end;

def checkMatching = \openCount. \offset.

    blarg <- scan down;
    let thing = blarg in

    bracketDirTemp <- case thing (\x. return $ inl x) bracketForBool;
    let bracketDir = bracketDirTemp in

    case bracketDir (\_.
        return (openCount == 0, offset);
    ) (\isOpening.
        if isOpening {

            move;
            checkMatching (openCount + 1) (offset + 1);
        } {
            if (openCount > 0) {

                move;
                checkMatching (openCount - 1) (offset + 1);
            } {
                move;
                return (false, offset);
            };
        };
    );
    end;

def checkLines = \lineWidth. \n.

    result <- checkMatching 0 0;
    let distanceTravelled = snd result in

    // We may have aborted early, so move the
    // rest of the way to the end of the line:
    doN (lineWidth - distanceTravelled - 1) move;


    let isValidBrackets = fst result in
    item <- bitFromBool isValidBrackets;
    place item;

    if (n > 1) {
        turn back;
        doN lineWidth move;
        turn left;
        move;
        turn left;

        checkLines lineWidth $ n - 1;
    } {};
    end;

def go = \lineCount. \lineWidth.

    _m <- listen;
    move;
    move;


    checkLines lineWidth lineCount;

    end;

go 10 20;