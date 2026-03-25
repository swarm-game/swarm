import "~swarm/lib/control"

def iter : Int -> Int -> (Int -> Cmd Unit) -> Cmd Unit = \n. \k. \f.
  if (n == k) {} {f k; iter n (k+1) f}
end

def placeAll = \c.
  let p = "paper: " ++ toChar (c + 65) in
  while (has p) {place p; move}
end

def go =
  doN 3 move;
  doN 16 (grab; move);
  turn back; doN 16 move; turn back;
  iter 26 0 placeAll
end
