def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def slowM = move; wait 2; end;

def navigatePath =
    doN 9 move;
    turn left;
    wait 13; // Get this timing right
    doN 12 slowM;
    turn right;
    doN 8 move;
    turn right;

    wait 0; // Get this timing right
    doN 12 slowM;
    turn left;
    doN 8 move;

    turn left;
    wait 0; // Get this timing right
    doN 12 slowM;
    turn left;
    move;

    k <- grab;
    equip k;

    doN 5 move;
    drill forward;

    doN 2 move;
    turn left;

    wait 0; // Get this timing right
    doN 4 move;
    turn right;
    drill forward;
    doN 2 move;

    // Close the door behind us!
    drill back;

    doN 4 move;
    turn left;
    doN 4 move;
    turn right;
    
    wait 4; // Get this timing right
    drill forward;
    doN 2 move;
    turn left;
    doN 4 move;
    turn right;
    doN 10 move;
    turn right;
    doN 12 move;
    turn right;
    move;
    drill forward;
    doN 7 move;
    grab;
    end;

navigatePath;