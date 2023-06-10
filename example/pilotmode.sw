def cons : a * b -> (a -> b) -> (a -> b) = \p. \k. \a.
  if (a == fst p) {snd p} {k a}
end

def nil : a -> cmd unit = \a. return () end

// Suitable to use as e.g.
//   installKeyHandler "(S-)←↓↑→ [Del] [g]rab [h]arvest [d]rill [s]can [b]locked [u]pload" pilot
def pilot : key -> cmd unit =
  cons (key "Up",      move) $
  cons (key "Down",    turn back) $
  cons (key "Left",    turn left) $
  cons (key "Right",   turn right) $
  cons (key "S-Up",    turn north) $
  cons (key "S-Down",  turn south) $
  cons (key "S-Left",  turn west) $
  cons (key "S-Right", turn east) $
  cons (key "Del",     selfdestruct) $
  cons (key "g",       res <- grab; log res) $
  cons (key "h",       res <- harvest; log res) $
  cons (key "d",       res <- drill forward; case res (\_. return ()) log) $
  cons (key "s",       res <- scan forward; case res (\_. return ()) log) $
  cons (key "b",       b <- blocked; if b {log "blocked"} {log "not blocked"}) $
  cons (key "u",       upload base) $
  nil
end
