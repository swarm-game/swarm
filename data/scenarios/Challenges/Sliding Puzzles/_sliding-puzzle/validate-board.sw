def id = \t. t end
def elif = \t. \then. \else. {if t then else} end
def else = id end

def itemIsHere = \item.
    x <- scan down;
    case x (\_. pure false) (\found. pure $ found == item);
    end;

def getOrdinal : Text -> Cmd Int = \item.
    count $ item ++ "-ordinal";
    end;

/**
  Cells are allowed to be either empty or a valid game tile.
  Returns a Left if we are non-monotonic.
  Otherwise returns the next expected value.
*/
def isMonotonic : Int -> Cmd (Unit + Int) = \expectedVal.
    maybeItem <- scan down;
    case maybeItem
        (\_. pure $ inR expectedVal) // Cell was blank
        (\entity.
            intVal <- getOrdinal entity;
            pure $ if (intVal == expectedVal) {
                inR $ expectedVal + 1;
            } {
                inL ();
            };
        );
    end;

/**
  Recurses over all cells in all rows.
  Traverses within rows via physical `move`-ment.
  Wraps to the next row via teleport if a border is encountered.
  Terminates if still on a border immediately after wrapping.

  Precondition: Facing east at location (0, 0).
*/
def loopMonotonicityCheck : Int -> Cmd Bool = \expectedVal.
    isOnBottomBorder <- itemIsHere "border";
    if isOnBottomBorder {
        pure true;
    } {
        maybeNextVal <- isMonotonic expectedVal;
        case maybeNextVal
            (\_. pure false)
            (\nextVal.
                move;
                isOnRightBorder <- itemIsHere "border";
                if isOnRightBorder {
                    loc <- whereami;
                    teleport self (0, snd loc - 1);
                } {};
                loopMonotonicityCheck nextVal;
            );
    }
    end;

def go =
    hasFlower <- has "flower";
    if hasFlower {
        turn east;
        teleport self (0, 0);
        loopMonotonicityCheck 1;
    } {
        pure false;
    };
    end;

go;
