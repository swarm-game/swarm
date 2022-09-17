def until = \p. \c. q <- p; if q {} {c; until p c} end;
def rep = \n. \c. if (n == 0) {} {c; rep (n-1) c} end;

def ifC = \p. \t. \e. res <- p; if res t e end;

def orC = \c1. \c2.
  b1 <- c1; b2 <- c2; return (b1 || b2)
end;

def somethingHere =
  res <- scan down;
  return (res != inl ())
end;

def fwdToThing = until blocked move end;

def fwdToBlank =
  move;
  until (orC blocked somethingHere) move;
  ifC somethingHere {turn back; move; turn back} {}
end;

def goBack = turn back; fwdToBlank; turn back end;

def getDisk =
  fwdToThing;
  d <- grab;
  goBack;
  return d
end;

def placeDisk = \d.
  fwdToBlank;
  place d;
  goBack
end;

def moveToCol = \x.
  w <- whereami;
  if (fst w < x) { turn east; rep (x - fst w) move }
  { if (fst w > x) { turn west; rep (fst w - x) move } {} };
  turn south
end;

def hanoi : int -> int -> int -> int -> cmd () =
  \n. \a. \b. \c.
  if (n == 0) {}
  {
    hanoi (n-1) a c b;
    moveToCol a;
    wait 64;
    d <- getDisk;
    moveToCol c;
    placeDisk d;
    hanoi (n-1) b a c;
  }
end;

hanoi 3 (-2) 0 2
