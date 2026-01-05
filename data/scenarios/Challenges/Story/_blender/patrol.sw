import "~swarm/lib/control"

def encircle = \lDir. \rDir.
    turn lDir;
    b <- blocked;
    if b {
        turn rDir;
    } {
        wait 1;
    };
    fwBlocked <- blocked;
    if fwBlocked {turn rDir} {move};
    end;

def patrolCW = forever (encircle right left); end;
def patrolCCW = forever (encircle left right); end;
