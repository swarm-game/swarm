def elif = \p.\t.\e. {if p t e} end;

def go : cmd (unit + dir) -> cmd unit = \gps.
md <- gps;
case md (\_.
    noop
) (\d.
    turn d;
    move;
    go gps
)
end;

def scanIf: dir -> (unit + dir) -> text -> cmd (unit + dir) =
    \d. \current. \e.
    sc <- scan d;
    if (sc == inr e) {
        if (current == inl ()) {
            // say ("I am going " ++ format d);
            return $ inr d
        } {
            // say "Multiple paths!";
            fail "Multiple paths!"
        }
    } {
        return $ current
    }
end;

def follow: text -> cmd (unit + dir) = \e.
    let noDir: unit + dir = inl () in
    f <- scanIf forward noDir e;
    fl <- scanIf left f e;
    flr <- scanIf right fl e;
    return flr;
end;

// follow a path from that ends right next to starting location
def broken_circle_north: text -> cmd bool = \e.
    turn north;
    b <- try {
        go (follow e); return true
    } {
        // say "failed to follow"; wait 8; selfdestruct;
        return false
    };
    if b {
        // end next to start
        l <- whereami;
        //say ("I am at" ++ format l);
        //selfdestruct;
        return $ l == (0,-1)
    } {
        return false
    }
end;


def untilBaaad: dir -> cmd (unit + dir) = \d.
//bl <- blocked;
//if bl {
//  return $ inl ()
//} {
    sf <- scan forward;
    if (sf == inr "water" || sf == inr "gate") {
        return $ inl ()
    } {
        return $ inr d
    }
//} 
end;

def checkWaterR: int -> cmd int = \n.
    try {
        r <- robotNumbered n;
        wN <- as r {go (untilBaaad north); sf <- scan forward; return $ sf == inr "water"};
        wS <- as r {go (untilBaaad south); sf <- scan forward; return $ sf == inr "water"};
        if (wS || wN) {return 0} {return 1}
    } {
        return 0; 
    }
end;

// WATCH PROOF OF CONCEPT
//
// this shows that waiting for entities in location saves most time
// -> checks only after the fence is connected to both sides
//
teleport self (1,2);
sd1 <- scan west;
teleport self (1,-2);
sd2 <- scan west;
teleport self (0,0);

if (sd1 == inr "fence" && sd2 == inr "fence") {
/**************************************/
    // SHEEP CHECK
    //
    // one sheep has to be surrounded by the
    // fence from top and bottom
    //
    sheepWater1 <- checkWaterR 1;
    if (sheepWater1 == 1) {
        broken_circle_north "fence"
    } {
        sheepWater2 <- checkWaterR 2;
        if (sheepWater2 == 1) {
            broken_circle_north "fence"
        } {
            sheepWater3 <- checkWaterR 3;
            if (sheepWater3 == 1) {
                broken_circle_north "fence"
            } {
                return false;
            }
        }
    }
/**************************************/
} {
    return false;
}
