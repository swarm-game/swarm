import "~swarm/lib/control"
import "common"

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
                    match loc \_. \y.
                    teleport self (0, y - 1);
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
