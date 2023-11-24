def goDir = \f. \d.
  if (d == down) {
    eggHere <- ishere "egg";
    if eggHere {grab; return ()} {};
    return ()
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
    case nextDir return $ goDir $ followRoute loc;
    end;

def visitNextWaypoint = \nextWpIdx.
    nextWaypointQuery <- waypoint "wp" nextWpIdx;
    followRoute $ snd nextWaypointQuery;

    visitNextWaypoint $ nextWpIdx + 1;
    end;

def go =
    visitNextWaypoint 0;
    end;

go;
