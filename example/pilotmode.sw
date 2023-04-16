def cons : a * b -> (a -> b) -> (a -> b) = \p. \k. \a.
  if (a == fst p) {snd p} {k a}
end

def nil : a -> cmd unit = \a. return () end

// Suitable to use as e.g.
//   installKeyHandler "(S-)←↓↑→ [g]rab [h]arvest [d]rill [s]can [u]pload" pilot
def pilot : key -> cmd unit =
  cons (key "Up",      move) $
  cons (key "Down",    turn back) $
  cons (key "Left",    turn left) $
  cons (key "Right",   turn right) $
  cons (key "S-Up",    turn north) $
  cons (key "S-Down",  turn south) $
  cons (key "S-Left",  turn west) $
  cons (key "S-Right", turn east) $
  cons (key "g",       grab; return ()) $
  cons (key "h",       harvest; return ()) $
  cons (key "d",       drill forward; return ()) $
  cons (key "s",       scan forward; return ()) $
  cons (key "u",       upload base) $
  nil
end
