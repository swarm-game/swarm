def until = \c. b <- c; if b {} {until c} end
def untilSafe = \c. until ( try { c } { return false } ) end

def orM = \p1.\p2. b1 <- p1; if b1 {return true} {p2} end
def abs = \x. if (x >= 0) {x} {-x} end

def baseIsAt = \l.
    loc <- as base {whereami};
    // TRICK:
    // we only check for base at this or neighbor location
    // so we can sleep for as long as base will take to get there
    let dist = abs (fst l - fst loc) + abs (snd l - snd loc) in
    if (dist > 2) {
        wait $ dist - 1
    } {};
    return (loc == l)
end

def room1 = 
    // l <- whereami;
    let l = (2, 0) in
    untilSafe (baseIsAt l);
    instant (
        create "Win";
        // open door
        turn east; move; grab;
    )
end

def room2 =
    let l = (8, 0) in
    teleport self l;
    untilSafe (baseIsAt l);
    instant (
        create "Win";
        // open doors
        turn north; move; grab;
        turn west; move; grab;
    )
end

def room3 =
    let l = (7, 4) in
    teleport self l;
    untilSafe $ orM (baseIsAt l) (baseIsAt (8, 4));
    instant (
        create "Win";
        // open door
        turn west; move; grab;
    )
end

def room4 =
    let l = (8, 8) in
    teleport self l;
    untilSafe (baseIsAt l);
    create "Win";
end

def main =
   room1;
   room2;
   room3;
   room4;
end

main