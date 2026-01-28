import "~swarm/lib/control"

def go =
  doN 3 move;
  f <- grab;

  doN 5 move;
  r <- meet;
  case r pure $ \j. give j f;
end
