let rep : int -> cmd () -> cmd () = \n. \c.
  if (n == 0)
    {}
    { c; rep (n-1) c }
in
rep 50 {
  harvest; move;
  turn left;
  harvest; move;
  turn right;
  y <- getY;
  if (y >= 10) {
    turn right
  } {
    if (y <= 0) { turn left } {}
  }
}