def forever: ∀ a b. {Cmd a} -> Cmd b = \c. force c; forever c end

def X : Int -> Cmd Unit -> Cmd Unit = \n. \c.
  if (n == 0) {} {c; X (n-1) c}
end

def pixel : (Int * Int) * Text -> Cmd Unit = \instr.
  match instr \loc. \ty.
  match loc \x. \y.
  turn back; X 5 move; turn right; X 2 move;
  turn west; X x move; turn north; X y move;
  place ty;
  turn south; X y move; turn east; X x move;
  X 5 move; turn right; X 2 move; turn east
end

def followInstructions : Text -> Cmd Unit = \paper.
  try {
    let res = read @((Int * Int) * Text) paper
    in  pixel res
  } {}
end

def copy : Cmd Unit =
  watch down; wait 1024;
  p <- atomic (b <- isempty; if b {pure ""} {grab});
  if (p == "") {} {followInstructions p}
end

def go = forever {copy} end

go;
