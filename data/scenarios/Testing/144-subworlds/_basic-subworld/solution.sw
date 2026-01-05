import "~swarm/lib/control"

def go =
  doN 8 move;
  f <- grab;
  doN 7 move;
  place f;
end
