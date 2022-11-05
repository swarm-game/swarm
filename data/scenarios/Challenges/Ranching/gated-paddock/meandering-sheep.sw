// A "sheep" that wanders around randomly.

let forever : cmd unit -> cmd unit = \c. c ; forever c in
let repeat : int -> cmd unit -> cmd unit =
  \n. \c. if (n == 0) {} {c ; repeat (n-1) c} in
let randdir : cmd dir =
  d <- random 4;
  return (
    if (d == 0) {north}
    {if (d == 1) {east}
    {if (d == 2) {south} {west}}})
  in

forever (
  n <- random 30;
  wait (30 + n);
  d <- randdir;
  turn d;
  dist <- random 3;
  repeat dist (
    b <- blocked;
    if b {} {
      move;

      // Sheep can drown.
      hasWater <- ishere "water";
      if hasWater {
        say "whoops!";
        selfdestruct;
      } {};

    }
  );
  r <- random 5;
  if (r == 0) { say "baaa" } {}
)
