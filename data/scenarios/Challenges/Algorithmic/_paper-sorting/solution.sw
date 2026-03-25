import "~swarm/lib/control"

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
