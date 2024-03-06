def doUntilCorrect =
    herenow <- ishere "dial (G)";
    if herenow {
        give base "flower";
    } {
        watch down;
        wait 1000;
        doUntilCorrect;
    }
    end;
    
def go =
    instant $
        doUntilCorrect;
    end;

go;
