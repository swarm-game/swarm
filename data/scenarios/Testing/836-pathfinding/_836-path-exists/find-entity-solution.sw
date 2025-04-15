def goDir = \f. \r.
  match r \d. \_.
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
