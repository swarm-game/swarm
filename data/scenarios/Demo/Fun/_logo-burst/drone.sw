import "~swarm/lib/control"
import "~swarm/lib/arith"

def randdir : Cmd Dir =
  d <- random 4;
  pure (
    if (d == 0) {north}
    $ elif (d == 1) {east}
    $ elif (d == 2) {south}
    $ else {west}
  )
end;

def wander =
  d <- randdir;
  turn d;
  dist <- random 4;
  try {doN dist move} {};
end;

def disperse =
  r <- robotnamed "stoplight";
  greenLight <- as r {has "bit (1)"};
  if greenLight {
    wander;
    disperse;
  } {};
  end;

def converge = \loc. \currentLoc.
  match loc \x. \y.
  match currentLoc \curx. \cury.
  let xDist = curx - x in
  let yDist = cury - y in

  if (xDist < 0) {
    turn east;
  } {
    if (xDist > 0) {
      turn west;
    } {};
  };
  doN (abs xDist) move;

  if (yDist < 0) {
    turn north;
  } {
    if (yDist > 0) {
      turn south;
    } {};
  };
  doN (abs yDist) move;
  end;

def go = \loc.
  disperse;
  currentLoc <- whereami;
  converge loc currentLoc;
  go loc;
  end;

def drone =
  loc <- whereami;
  go loc;
end
