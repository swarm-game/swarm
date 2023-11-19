def goDir = \f. \d.
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
