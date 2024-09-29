def elif = \b.\t.\e. {if b t e} end
def else = \e. e end

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

def wait_until_change = \pos. \original. \cont.
    surveil pos;
    wait 1024;
    current <- as self {teleport self pos; scan down};
    if (current != original) {
        cont current;
    } {
        wait_until_change pos original cont
    }
end

def main: [entity: Text, room: Int] -> Cmd Unit = \args.
    let pos = position args.room in
    log $ format args;
    original <- as self {teleport self pos; scan down};
    log $ "surveil position" ++ format pos ++ ": " ++ format original;
    create args.entity;
    log $ "sleeping until " ++ format pos ++ " changes";
    wait_until_change pos original ( \after_change.
        // for later rooms a wall appears and then a door is opened
        if (args.room == 1) {
            place args.entity
        } {
            log $ "sleeping until " ++ format pos ++ " changes again";
            wait_until_change pos after_change (\_. place args.entity)
        };
    )
end