
def waitUntilUnblocked =
    x <- blocked;
    if x {waitUntilUnblocked} {};
    end;

def solve =
    waitUntilUnblocked;
    move;
    move;
    drill down;
    move;
    drill down;
    move;
    drill down;
    move;
    end;

solve;


      