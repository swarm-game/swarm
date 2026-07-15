def ifC: ∀ a. Cmd Bool -> {Cmd a} -> {Cmd a} -> Cmd a
  = \test. \then. \else.
  b <- test;
  if b then else
end

def while: ∀ a. Cmd Bool -> {Cmd a} -> Cmd Unit
  = \test. \body.
  ifC test {force body; while test body} {}
end

def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {} end

def upTo_ : Int -> Int -> (Int -> Cmd a) -> Cmd Unit = \n. \k. \f.
  if (n == k) {} {f k; upTo_ n (k+1) f}
end

def placeAll = \c.
  let p = "paper: " ++ toChar (c + 65) in
  while (has p) {place p; move}
end

def go =
  doN 3 move;
  doN 16 (grab; move);
  turn back; doN 16 move; turn back;
  upTo_ 26 0 placeAll
end
