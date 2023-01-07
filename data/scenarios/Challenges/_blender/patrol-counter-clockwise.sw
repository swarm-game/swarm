/**
Don't cross unlocked doors
*/
def isBlocked =
    b <- blocked;
    x <- scan forward;
    let isUnlockedDoor = case x (\_. false) (\z. z == "unlocked door") in
    return $ b || isUnlockedDoor;
    end;

def forever = \c.
    c;
    forever c;
    end;

def encircle = \lDir. \rDir.
    turn lDir;
    b <- isBlocked;
    if b {
        turn rDir;
    } {
        wait 1;
    };
    
    move;
    end;

def patrolCCW = forever (encircle right left); end;

patrolCCW;