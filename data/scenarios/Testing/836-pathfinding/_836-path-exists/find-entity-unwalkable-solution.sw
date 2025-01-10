def goDir = \goalItem. \f. \r.
  let d = fst r in
  if (d == down) {
    grab; pure ()
  } {
    turn d;
    itemAhead <- scan forward;
    let isGoalAhead = case itemAhead (\_. false) (\item. item == goalItem) in
    if isGoalAhead {
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

followRoute;
