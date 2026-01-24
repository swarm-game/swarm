import "../../../lib/control"
import "../../../lib/arith"

def cnt = \x.
  if (x == inl ()) {0} {1}
end;

def count3 =
  h <- scan down;
  f <- scan forward;
  b <- scan back;
  pure (cnt h + cnt f + cnt b)
end;

def waitUntil = \p.
  b <- p;
  if b {wait 1} {waitUntil p}
end;

def main = 
  forever (
    h <- scan down;
    alive <- pure (h != inl ());
    n1 <- count3;
    turn left; move; turn right; n2 <- count3;
    turn right; move; move; turn left; n3 <- count3;
    turn left; move; turn right;
    total <- pure (n1 + n2 + n3 - if alive {1} {0});
    if (alive && (total < 2 || total > 3))
      { grab; pure () }
      { if (not alive && total == 3)
        { place "rock" }
        {}
      };
    // synchronize
    waitUntil (t <- time; pure (mod t 0x20 == 0))
  )
end