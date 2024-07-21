def repeat : Int -> (Int -> Cmd Unit) -> Cmd Unit =
  \n. \c. if (n == 0) {} {c n; repeat (n-1) c}
end

def elif = \b.\t.\e. {if b t e} end
def else = \e. e end

def act_lazy: Text -> Text -> Cmd (Cmd Unit) = \a.\e. instant $
    if (a == "S") {
        if (e != "") { create e } {};
        return (swap e; log $ a ++ ": " ++ e)
    } $elif (a == "G") {
        return (grab; log a)
    } $elif (a == "P") {
        if (e != "") { create e } {};
        return (place e; log $ a ++ ": " ++ e)
    } $else {
        return (fail $ "Finished waiting for check but I don't know what to do: '" ++ a ++ "'")
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

def room_changes = \room. if (room == 1) {1} {2} end

def main: [action: Text, entity: Text, room: Int] -> Cmd (Cmd Unit) = \args.
    let pos = position args.room in
    log $ format args;
    target <- as self {teleport self pos; scan down};
    log $ "at position" ++ format pos ++ ": " ++ format target;
    act <- act_lazy args.action args.entity;
    return (
        // first room optimization - actively wait for one tick to see if the change occured
        target2 <- as self {teleport self pos; scan down};
        if (args.room == 1 && target == target2) {
            turn forward
        } {};
        let changes = room_changes args.room in
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
        repeat (changes - changed) (\i.
            log $ "sleeping until " ++ format pos ++ " changes (countdown: " ++ format i ++ ")";
            surveil pos;
            wait 1000000;
        );
        act
    )
end