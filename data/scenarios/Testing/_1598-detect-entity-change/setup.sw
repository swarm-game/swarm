def checkCombo = \colorString.
    herenow <- ishere colorString;
    if herenow {
        return true;
    } {
        watch down;
        return false;
    }
    end;

def doUntilCorrect =
    isCorrect <- instant $ checkCombo "dial (G)";
    if isCorrect {
        give base "flower";
    } {
        wait 1000;
        doUntilCorrect;
    };
    end;
    
def go =

    doUntilCorrect;
    end;

go;
