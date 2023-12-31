def goDir = \f. \r.
  let d = fst r in
  if (d == down) {
    grab; return ()
  } {
    turn d; move; f;
  }
  end;

def followRoute =
    nextDir <- path (inL ()) (inR "flower");
    case nextDir return $ goDir followRoute;
    end;

followRoute;
