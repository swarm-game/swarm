/* MOVE TUTORIAL - PLACEMENT ROBOT PROGRAM

The wall tutorial places entities as the player progresses
through the rooms. The difficulty arises from the rooms
being so small (we don't want to overwhelm the player)
that the next room has to be placed in an INSTANT.

To solve this the program returns peculiar type Cmd (Cmd Unit).
The program is instantly run by the robot so the run and setup
is guaranteed to happen in one tick and return the rest of the computation.

1. SETUP - scans the location and create the entity
2. SURVEIL - wait for the location to change
3. PLACE the entity
*/

def repeat : Int -> (Int -> Cmd Unit) -> Cmd Unit =
  \n. \c. if (n == 0) {} {c n; repeat (n-1) c}
end

def elif = \b.\t.\e. {if b t e} end
def else = \e. e end

def safe_place: Text -> Cmd Unit = \e. 
   try {
    place e; log $ "placed: " ++ e
   } {
    log $ "ERROR: could not place: " ++ e
   }
end

def position: Int -> (Int * Int) = \room.
    if (room == 1) {
        (3,0)
    } $elif (room == 2) {
        (7,1)
    } $elif (room == 3) {
        (6,4)
    } $else {
        fail $ "unknown room: " ++ format room
    }
end

def main: [entity: Text, room: Int] -> Cmd (Cmd Unit) = \args.
    let pos = position args.room in
    log $ format args;
    target <- as self {teleport self pos; scan down};
    log $ "at position" ++ format pos ++ ": " ++ format target;
    create args.entity;
    return (
        // first room optimization - actively wait for one tick to see if the change occured
        target2 <- as self {teleport self pos; scan down};
        if (args.room == 1 && target == target2) {
            turn forward
        } {};
        has_changed <-
            if (target != target2) {
                log "changed in first tick - skipping one wait";
                return true
            } $else {
                target3 <- as self {teleport self pos; scan down};
                if (target != target3) { log "changed in second tick - skipping one wait" }{};
                return $ target != target3
            };
        let changed = if has_changed {1} {0} in
        // for later rooms a wall appears and then a door is opened
        let change_count = if (args.room == 1) {1} {2} in
        repeat (change_count - changed) (\i.
            log $ "sleeping until " ++ format pos ++ " changes (countdown: " ++ format i ++ ")";
            surveil pos;
            wait 1000000;
        );
        safe_place args.entity
    )
end