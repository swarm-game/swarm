def isItemInDirection = \direction. \item.
    x <- scan direction;
    pure $ case x (\_. false) (\y. y == item);
    end;

def isFlankedByItem = \item.
    hasLeft <- isItemInDirection left item;
    hasRight <- isItemInDirection right item;
    pure $ hasLeft && hasRight;
    end;

def flowersInCardinalDirections = \item. \n.
    if (n > 0) {
        x <- isItemInDirection forward item;
        if x {
            turn left;
            flowersInCardinalDirections item $ n - 1;
        } {
            pure false;
        };
    } {
        pure true;
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
            pure isFlanked2;
        } {
            pure false;
        }
    } {
        pure false;
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

def go =
  waitUntilSurrounded;

  _t <- swap "painted plate";
  selfdestruct;
end
