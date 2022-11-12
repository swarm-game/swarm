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

def moveToCol = \w.\x.
  if (w < x) { turn east; rep (x - w) move }
  { if (w > x) { turn west; rep (w - x) move } {} };
  turn south
end;

def hanoi :
  int -> // The number of disks in each column
  int -> // Current column (basically offset of all columns)
  int -> // The offset to first column
  int -> // The offset to second column
  int -> // The offset to third column
  cmd int
  = \n. \o. \a. \b. \c.
  if (n == 0) {return o}
  {
    o_new <- hanoi (n-1) o a c b;
    moveToCol o_new a;
    wait 8;
    d <- getDisk;
    moveToCol a c;
    placeDisk d;
    hanoi (n-1) c b a c;
  }
end;

hanoi 3 0 (-2) 0 2
