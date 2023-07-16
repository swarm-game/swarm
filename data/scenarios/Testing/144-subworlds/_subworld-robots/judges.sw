
def getRobotNumber = \n.
    r <- robotnumbered n;
    if (r == self) {
        return n;
    } {getRobotNumber $ n + 1};
    end;

def amLowestRecursive = \targetName. \idx.
    r <- robotnumbered idx;
    thisName <- as r {whoami};
    if (thisName == targetName) {
        return $ r == self;
    } {amLowestRecursive targetName $ idx + 1};
    end;

/**
Iterates through robots by increasing index.
If we encounter a robot, fetched by index,
with the same name as me, but I am not that robot,
then we return false.
*/
def amFirstOfMyName =
    myName <- whoami;
    amLowestRecursive myName 0;
    end;

def waitToGiveThing = \thing.
    r <- meet;
    case r (\_. wait 1; waitToGiveThing thing) $ \b. give b thing;
    end;

def waitToGive =
    let thing = "bitcoin" in
    create thing;
    waitToGiveThing thing;
    end;

def waitToReceive =
    noop;
    end;

def go =
    myNumber <- getRobotNumber 0;
    log $ "My number: " ++ format myNumber;
    amFirst <- amFirstOfMyName;
    log $ "Am first with this name? " ++ format amFirst;

    if amFirst {waitToReceive} {waitToGive};
    end;

go;