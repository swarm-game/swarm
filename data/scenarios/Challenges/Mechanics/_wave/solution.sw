def go =
    move;
    go;
    end;

def start =
    turn right;
    wait 5;
    try {
        go;
    } {};
    grab;
    end;

start;