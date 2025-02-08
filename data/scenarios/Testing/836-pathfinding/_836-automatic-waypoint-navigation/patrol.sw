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

def visitNextWaypoint = \nextWpIdx.
    let nextWaypointQuery = waypoint "wp" nextWpIdx in
    followRoute $ nextWaypointQuery;

    visitNextWaypoint $ nextWpIdx + 1;
    end;

def go =
    visitNextWaypoint 0;
    end;

go;
