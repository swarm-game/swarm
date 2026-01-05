// 562-lodestone solution

import "~swarm/lib/control"

def waitFor = \e.\t.
  watch down;
  wait 100;
  ifC (ishere e) t {waitFor e t}
end;

def m2 =
  move;
  move
end;

// ---------------------------------------------------
// ┌─────┐
// │o.AT~ 
// │..AAA│
// │B.0.A│
// └─────┘
// ---------------------------------------------------

def solution =
  // get one lodestone
  build {log "Hey!"; turn north; m2; l <- grab; turn back; m2; place l};
  waitFor "lodestone" {grab};

  // get two bit (0)
  build {
    log "Hi!";
    forever (
      log "I am going for a bit";
      turn east; m2; x <- waitFor "bit (0)" {harvest}; turn back; m2; place x;
                  log "I brought a bit";
  )};
  waitFor "bit (0)" {grab};
  waitFor "bit (0)" {grab};

  make "bit (1)";
  make "drill bit"
end
