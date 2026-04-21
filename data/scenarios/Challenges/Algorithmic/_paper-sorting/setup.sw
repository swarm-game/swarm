import "~swarm/lib/control"

def setup =
  instant {
    let a = charAt 0 "A" in
    doN 16 (
      n <- random 26;
      create "paper";
      p <- print "paper" (toChar (a + n));
      place p;
      move
    );
    turn back; doN 16 move; turn back;
    create "bitcoin"
  }
end
