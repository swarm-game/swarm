def ifC : Cmd Bool -> {Cmd a} -> {Cmd a} -> Cmd a = \test. \then. \else.
  b <- test; if b then else
end

def when : Cmd Bool -> Cmd a -> Cmd Unit = \test. \cmd.
  b <- test;
  if b {cmd; pure ()} {}
end

def x2 = \c. c;c end
def x4 = \c. x2 c; x2 c end
def x6 = \c. x4 c; x2 c end
def x8 = \c. x4 c; x4 c end
def m16 = x4 (x4 move) end

def copyWith : Text -> Cmd Unit = \pix.
  m16; place pix; turn back; m16; turn back
end

def copy : Cmd Unit =
  ifC isEmpty {copyWith "pixel (B)"} {copyWith "pixel (W)"}
end

def copyRow = x8 (copy; move) end

def copyBlock = x6 (copyRow; turn back; x8 move; turn left; move; turn left) end

def solve = turn right; copyBlock end

solve
