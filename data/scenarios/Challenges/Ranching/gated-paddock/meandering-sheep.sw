// A "sheep" that wanders around randomly.

/** A "gate" is walkable, so we need to supplement the "blocked" check with this function.
Since fences are "unwalkable", they do not need to be mentioned in this function.
*/
def isFenced =
    s <- scan forward;
    return (
        case s
            (\_. false)
            (\x. x == "gate")
    );
    end;

def isBlockedOrFenced =
    b <- blocked;
    f <- isFenced;
    return (b || f);
    end;


def elif = \p.\t.\e. {if p t e} end;

let forever : cmd unit -> cmd unit = \c. c ; forever c in
let repeat : int -> cmd unit -> cmd unit =
  \n. \c. if (n == 0) {} {c ; repeat (n-1) c} in
let randdir : cmd dir =
  d <- random 4;
  return $ if (d == 0) {
      north
    } $ elif (d == 1) {
      east
    } $ elif (d == 2) {
      south
    } {
      west
    }
  in

forever (
  n <- random 30;
  wait (30 + n);
  d <- randdir;
  turn d;
  dist <- random 3;
  repeat dist (

    b <- isBlockedOrFenced;
    if b {} {
      move;
    };

    // Sheep can drown.
    hasWater <- ishere "water";
    if hasWater {
      say "whoops!";
      selfdestruct;
    } {};
  );
  r <- random 5;
  if (r == 0) { say "baaa" } {}
)
