def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;

def go =
    wait 2;
    equip "treads";
    turn back;
    doN 64 move;
    unequip "antenna";
    end;

go;