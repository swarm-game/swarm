def goDir = \f. \r.
  let d = fst r in
  if (d == down) {
    eggHere <- ishere "egg";
    if eggHere {grab; pure ()} {};
    pure ()
  } {
    turn d;

    // An obstruction might arise after
    // navigation direction is determined
    // but before we move.
    try {
      move;
    } {};
    f;
  }
  end;

def followRoute = \loc.
    nextDir <- path (inL ()) (inL loc);
    case nextDir pure $ goDir $ followRoute loc;
    end;


def visitNextWaypoint : (rec l. Unit + (Int * Int) * l) -> (rec l. Unit + (Int * Int) * l) -> Cmd Unit = \originalList. \remainingList.

    // Wrap around
    let myList = case remainingList (\_. originalList) (\_. remainingList) in

    case myList pure (\cons.
        followRoute $ fst cons;
        visitNextWaypoint originalList $ snd cons;
    );
    end;

def go =
    let wps = waypoints "wp" in
    visitNextWaypoint wps wps;
    end;

go;
