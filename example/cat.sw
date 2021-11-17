let forever : cmd () -> cmd () = \c. c ; forever c in
let repeat : int -> cmd () -> cmd () =
  \n. \c. if (n == 0) {} {c ; repeat (n-1) c} in
let randdir : cmd dir =
  d <- random 4;
  return (
    if (d == 0) {north}
    {if (d == 1) {east}
    {if (d == 2) {south} {west}}})
  in

forever (
  n <- random 20;
  wait (10 + n);
  d <- randdir;
  turn d;
  dist <- random 10;
  repeat dist move;
  r <- random 5;
  if (r == 0) { say "meow" } {}
)
