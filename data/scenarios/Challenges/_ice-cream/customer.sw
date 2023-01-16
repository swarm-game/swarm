def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def approachShop =
    doN 5 (
        move;
        wait 8;
    );
    end;

def placeOrder =
    scoopCountExtra <- random 4000;
    let scoopCount = 1000 + scoopCountExtra in
    say $ format scoopCount ++ " scoops of vanilla ice cream, no more, no less! And top it with a cherry, please.";
    return scoopCount;
    end;

def waitForItem = \item.
    hasItem <- has item;
    if hasItem {
        return ();
    } {
        wait 1;
        waitForItem item;
    }
    end;

/**
If we receive the cherry
when we also have the correct number of
scoops, then we produce this item
as a sempahore for the goal evaluation.

Otherwise, we express our disappointment.
*/
def createCompletedSemaphore = \targetCount.
    scoopcount <- count "scoop";
    if (scoopcount < targetCount) {
        say "That's not enough ice cream! Terrible service.";
    } {
        if (scoopcount > targetCount) {
            say "That's too much ice cream!";
        } {
            say "Just right! Here's your money.";
            give base "bitcoin";
        };
    };
    turn back;
    approachShop;
    selfdestruct;
    end;

def runScript =
    approachShop;
    waitForItem "cone";
    targetScoopCount <- placeOrder;
    waitForItem "cherry";
    createCompletedSemaphore targetScoopCount;
    end;

runScript;