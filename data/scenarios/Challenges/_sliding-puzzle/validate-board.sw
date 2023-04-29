def id = \t. t end
def elif = \t. \then. \else. {if t then else} end
def else = id end

def mod : int -> int -> int = \i.\m.
  i - m * (i / m)
end

def getOrdinal : text -> cmd int = \item.
    count $ item ++ "-ordinal";
    end;

/**
  Cells are allowed to be either empty or a valid game tile.
  Returns a Left if we are non-monotonic.
  Otherwise returns the next expected value.
*/
def isMonotonic : int -> cmd (unit + int) = \expectedVal.
    maybeItem <- scan down;
    case maybeItem
        (\_. return $ inR expectedVal) // Cell was blank
        (\entity.
            intVal <- getOrdinal entity;
            if (intVal == expectedVal) {
                return $ inR $ expectedVal + 1;
            } {
                return $ inL ();
            };
        );
    end;

/**
  Recurses over all cells in all rows.
  Moves by teleportation. Uses modulus
  to traverse between rows.
*/
def loopMonotonicityCheck : int -> int -> int -> int -> cmd bool = \boardWidth. \boardHeight. \idx. \expectedVal.
    if (idx < boardWidth * boardHeight) {
        let x = mod idx boardWidth in
        let y = idx / boardWidth in
        let coords = (x, -y) in
        teleport self coords;
        maybeNextVal <- isMonotonic expectedVal;
        case maybeNextVal
            (\_. return false)
            (\nextVal.
                loopMonotonicityCheck boardWidth boardHeight (idx + 1) nextVal;
            );
    } {
        return true;
    }
    end;

loopMonotonicityCheck 4 4 0 1;