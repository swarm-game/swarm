def ifC : Cmd Bool -> {Cmd a} -> {Cmd a} -> Cmd a = \test. \then. \else.
  b <- test; if b then else
end

def when : Cmd Bool -> Cmd a -> Cmd Unit = \test. \cmd.
  b <- test;
  if b {cmd; pure ()} {}
end

def x4 = \c. c;c;c;c end
def x6 = \c. x4 c; c; c end
def x10 = \c. x4 c; x6 c end
def m16 = x4 (x4 move) end
def m17 = move; m16 end

def copyWith : Text -> Cmd Unit = \pix.
  m17; place pix; turn back; m17; turn back
end

def copy : Cmd Unit =
  ifC isEmpty {copyWith "pixel (B)"} {copyWith "pixel (W)"}
end

def copyRow = x10 (copy; move) end

def copyBlock = x6 (copyRow; turn back; x10 move; turn left; move; turn left) end

def solve = turn right; copyBlock end

solve
