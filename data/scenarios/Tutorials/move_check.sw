def until = \cond. \c. b <- cond; if b {} {c; until cond c} end
def abs = \x. if (x >= 0) {x} {-x} end

// TRICK:
// we only check for base at this location
// so we can sleep for as long as base will take to get here
def get_dist = \l. \bl.
  abs (fst l - fst bl) + abs (snd l - snd bl)
end

def waitForBaseAt = \l. \get_timeout.
    loc <- as base {whereami};
    if (loc == l) {
        return true
    } {
        wait (get_timeout l loc);
        waitForBaseAt l get_timeout
    }
end

def room1 = 
    // l <- whereami;
    let l = (2, 0) in
    waitForBaseAt l (\_. \_. 1);
    log "room 1 done";
    // open door
    turn east; move; grab;
end

def room2 =
    let l = (8, 0) in
    teleport self l;
    waitForBaseAt l get_dist;
    log "room 2 done";
    // open doors
    turn north; move; grab;
    turn west; move; grab;
end

def room3 =
    let l = (8, 4) in
    teleport self l;
    waitForBaseAt l get_dist;
    log "room 3 done";
    // open door
    turn west; move; move; grab;
end

def main =
   room1;
   room2;
   room3;
end

main