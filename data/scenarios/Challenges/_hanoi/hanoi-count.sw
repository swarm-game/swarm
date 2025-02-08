def repeat = \c. force c; repeat c end;

def i2e = \i.
  if (i == 2) { "two" } {
  if (i == 3) { "three" } {
  fail $ "Fatal error: There should be only 2 or 3 entities placed at any time: " ++ format i
  }}
end;

def cscan = \d.
  s <- scan d;
  if (s == inl ()) {pure 0} {pure 1}
end;

def count_column =
  i <- cscan north;
  j <- cscan down;
  k <- cscan south;
  // log "one column";
  // wait 8;
  // log (format i);
  // wait 8;
  // log (format j);
  // wait 8;
  // log (format k);
  // wait 8;
  pure (i + j + k)
end;


repeat {
    sum <- as self {
        // left column
        teleport self (-2,-2);
        x <- count_column;
        // middle column
        teleport self (0,-2);
        y <- count_column;
        // right column
        teleport self (2,-2);
        z <- count_column;
        // DEBUG
        // log "all columns";
        // wait 8;
        // log (format x);
        // wait 8;
        // log (format y);
        // wait 8;
        // log (format z);
        pure $ i2e (x + y + z)
    };
    //let sum = i2e (x + y + z) in
    teleport self (0,-6);
    counted <- scan down;
    //wait 8;
    //log (format counted);
    case counted (\e.
        fail $ "Fatal error: there should always be a count entity at (0,-6)! " ++ format e ++ " " ++ format counted
    ) (\e.
        if (e == sum) {} {swap sum; pure ()}
    )
}
