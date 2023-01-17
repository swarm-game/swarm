
def repeat : int -> cmd unit -> cmd unit =
  \n. \c. if (n == 0) {} {c ; repeat (n-1) c}
end
def abs = \n. if (n < 0) {-n} {n} end
def elif = \t. \then. \else. {if t then else} end
def else = \t. t end

def randdir : cmd dir =
  d <- random 4;
  return (
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
  try {repeat dist move} {};
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

  let xDist = fst currentLoc - fst loc in
  let yDist = snd currentLoc - snd loc in

  if (xDist < 0) {
    turn east;
  } {
    if (xDist > 0) {
      turn west;
    } {};
  };
  repeat (abs xDist) move;

  if (yDist < 0) {
    turn north;
  } {
    if (yDist > 0) {
      turn south;
    } {};
  };
  repeat (abs yDist) move;
  end;

def go = \loc.
  disperse;
  currentLoc <- whereami;
  converge loc currentLoc;
  go loc;
  end;

loc <- whereami;
go loc;