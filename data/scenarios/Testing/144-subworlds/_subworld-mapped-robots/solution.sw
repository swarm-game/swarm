import "~swarm/lib/control"

def go =
  doN 16 move;

  r <- meet;
  case r pure $ \j. give j "bitcoin";
end
