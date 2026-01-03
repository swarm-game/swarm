import "~swarm/lib/control"

def pixel : (Int * Int) * Text -> Cmd Unit = \instr.
  match instr \loc. \ty.
  match loc \x. \y.
  turn back; doN 5 move; turn right; doN 2 move;
  turn west; doN x move; turn north; doN y move;
  place ty;
  turn south; doN y move; turn east; doN x move;
  doN 5 move; turn right; doN 2 move; turn east
end

def followInstructions : Text -> Cmd Unit = \paper.
  try {
    let res = read @((Int * Int) * Text) paper
    in  pixel res
  } {}
end

def copy : Cmd Unit =
  watch down; wait 1024;
  p <- atomic {b <- isempty; if b {pure ""} {grab}};
  if (p == "") {} {followInstructions p}
end

def go = forever copy end
