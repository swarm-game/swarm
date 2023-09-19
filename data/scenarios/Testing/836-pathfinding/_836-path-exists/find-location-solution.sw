def goDir = \f. \d.
  if (d == down) {
    grab; return ()
  } {
    turn d; move; f;
  }
  end;

def followRoute =
    nextDir <- path (inL ()) (inL (4, 0));
    case nextDir return $ goDir followRoute;
    end;

followRoute;
