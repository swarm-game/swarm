import "~swarm/lib/control"
import "~swarm/lib/array"
import "~swarm/lib/tuple"
import "perm"

def setup : Cmd (Array Int) =
  let alphabet = unfoldArray 0 (\i. if (i == 26) {inl()} {inr (i, i+1)}) in
  log "shuffling!";
  perm <- shuffle alphabet;
  log "shuffled!";
  upTo_ 26 0 (\i. let x = toChar (65 + indexArray perm i) in log x; create x; place x; move);
  turn back; doN 26 move; turn back;
  pure perm
end

def checkRow : Int -> Array Int -> Cmd Bool = \i. \sol.
  if (i == arraySize sol)
    {pure true}
    { res <- scan down;
      case res
        (\_. pure false)
        (\here. if (here == toChar (indexArray sol i + 65)) { move; checkRow (i+1) sol } { pure false })
    }
end

def returnToOrigin : Cmd Unit =
  turn west;
  until (loc <- whereami; pure (fst loc == 0)) move;
  turn east
end

def judge : Int -> Array Int -> Cmd Unit = \n. \perm.
  let sol = iteratePerm (n+1) perm
  in
    forever (
      instant {
        ok <- checkRow 0 sol;
        if ok {create "bitcoin"} {};
        returnToOrigin
      };
      wait 1
    )
end
