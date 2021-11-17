let forever : cmd () -> cmd () =
  \c. c ; forever c in
forever (
  b <- random 2;
  turn (if (b == 0) {left} {right});
  move
)
