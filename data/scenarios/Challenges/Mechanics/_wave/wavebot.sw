def crossPath =
    move; move; move; move; move; move;
    turn back;
    wait 5;
    end;

def go =
    crossPath;
    go;
    end;

def start =
    pos <- whereami;
    match pos \x. \_.
    wait x;
    go;
    end;