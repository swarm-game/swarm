def serveCustomer = \cone. \customer.
    give customer cone;
    end;

def getCounter =
    move;
    c <- grab;
    equip c;
    turn back;
    move;
    end;

def getBriefcase =
    turn right;
    move;
    move;
    b <- grab;
    equip b;
    turn back;
    move;
    move;
    end;

def getCone =
    move;
    move;
    cone <- grab;
    move;
    cherry <- grab;
    turn back;
    move;
    move;
    move;
    turn right;
    return (cone, cherry);
    end;

def meetCustomer =
    maybeCustomer <- meet;
    case maybeCustomer (\_. meetCustomer) return;
    end;

def serveScoop = \customer.
    let s = "scoop" in
    make s;
    give customer s;
    end;

def scoopUntil = \customer. \targetRemainingIngredientCount.
    currentCount <- count "milk";
    if (currentCount > targetRemainingIngredientCount) {
        serveScoop customer;
        scoopUntil customer targetRemainingIngredientCount;
    } {};
    end;

def runSolution = \targetRemainingIngredientCount.
    getCounter;
    getBriefcase;
    coneAndCherry <- getCone;
    let cone = fst coneAndCherry in
    let cherry = snd coneAndCherry in
    customer <- meetCustomer;
    serveCustomer cone customer;

    scoopUntil customer targetRemainingIngredientCount;
    give customer cherry;
    end;

runSolution 2712;