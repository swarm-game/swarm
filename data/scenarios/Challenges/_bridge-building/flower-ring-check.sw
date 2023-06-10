def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;


def isItemInDirection = \direction. \item. 
    x <- scan direction;
    return $ case x (\_. false) (\y. y == item);
    end;

def isFlankedByItem = \item.
    hasLeft <- isItemInDirection left item;
    hasRight <- isItemInDirection right item;
    return $ hasLeft && hasRight;
    end;

def flowersInCardinalDirections = \item. \n.
    if (n > 0) {
        x <- isItemInDirection forward item;
        if x {
            turn left;
            flowersInCardinalDirections item $ n - 1;
        } {
            return false;
        };
    } {
        return true;
    }
    end;

def flowersAround = \item.
    hasCardinals <- flowersInCardinalDirections item 4;
    if hasCardinals {
        move;
        isFlanked1 <- isFlankedByItem item;
        turn back;
        move;
        if isFlanked1 {
            move;
            isFlanked2 <- isFlankedByItem item;
            turn back;
            move;
            return isFlanked2;
        } {
            return false;
        }
    } {
        return false;
    }
    end;

// loop until condition is met
def waitUntilSurrounded =
    isSurrounded <- flowersAround "flower";
    if isSurrounded {} {
        wait 2;
        waitUntilSurrounded;
    }
    end;

waitUntilSurrounded;

_t <- swap "painted plate";
selfdestruct;
