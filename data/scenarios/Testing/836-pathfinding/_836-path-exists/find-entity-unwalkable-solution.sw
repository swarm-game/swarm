def goDir = \goalItem. \f. \d.
  if (d == down) {
    grab; return ()
  } {
    turn d;
    itemAhead <- scan forward;
    let isGoalAhead = case itemAhead (\_. false) (\item. item == goalItem) in
    if isGoalAhead {
      return ();
    } {
      move; f;
    };
  }
  end;

def followRoute =
    let goalItem = "water" in
    nextDir <- path (inL ()) (inR goalItem);
    case nextDir return $ goDir goalItem followRoute;
    end;

followRoute;
