def goDir = \f. \r.
  let d = fst r in
  if (d == down) {
    grab; pure ()
  } {
    turn d; move; f;
  }
  end;

def followRoute =
    nextDir <- path (inL ()) (inR "flower");
    case nextDir pure $ goDir followRoute;
    end;

followRoute;
