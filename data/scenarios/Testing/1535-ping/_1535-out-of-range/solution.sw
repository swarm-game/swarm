import "~swarm/lib/control"

def go =
    wait 2;
    equip "treads";
    turn back;
    doN 64 move;
    unequip "antenna";
    end;
