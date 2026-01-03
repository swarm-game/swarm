import "~swarm/lib/control"

def singleLeg =
    doN 4 move;
    turn left;
    end;

def singleLoop =
    doN 4 singleLeg;
    end;

def solution =
  doN 3 singleLoop;

  doN 2 move;
  turn left;
  doN 2 move;
  grab;
end
