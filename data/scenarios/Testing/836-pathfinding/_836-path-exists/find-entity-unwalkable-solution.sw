def goDir = \goalItem. \f. \r.
  match r \d. \_.
  if (d == down) {
    grab; pure ()
  } {
    turn d;
    itemAhead <- scan forward;
    if (itemAhead == goalItem) {
      pure ();
    } {
      move; f;
    };
  }
  end;

def followRoute =
    let goalItem = "water" in
    nextDir <- path (inL ()) (inR goalItem);
    case nextDir pure $ goDir goalItem followRoute;
    end;
