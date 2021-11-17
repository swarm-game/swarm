let rep : int -> cmd () -> cmd () = \n. \c.
  if (n == 0)
    {}
    { c; rep (n-1) c }
in
turn east;
rep 50 (
  grab; move;
  turn left;
  grab; move;
  turn right;
  p <- whereami;
  let y = snd p in
  if (y >= 10) {
    turn right
  } {
    if (y <= 0) { turn left } {}
  }
)
