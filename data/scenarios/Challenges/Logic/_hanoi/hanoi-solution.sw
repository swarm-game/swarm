import "~swarm/lib/control"

def somethingHere =
  res <- scan down;
  pure (res != inl ())
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
  pure d
end;

def placeDisk = \d.
  fwdToBlank;
  place d;
  goBack
end;

def moveToCol = \w.\x.
  if (w < x) { turn east; doN (x - w) move }
  { if (w > x) { turn west; doN (w - x) move } {} };
  turn south
end;

def hanoi :
  Int -> // The number of disks in each column
  Int -> // Current column (basically offset of all columns)
  Int -> // The offset to first column
  Int -> // The offset to second column
  Int -> // The offset to third column
  Cmd Int
  = \n. \o. \a. \b. \c.
  if (n == 0) {pure o}
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
