
def visitNextWaypoint = \nextWpIdx.
    emptyHere <- isempty;
    if emptyHere {
      try {
        place "egg";
      } {};
    } {};
    watch down;
    let nextWaypointQuery = waypoint "wp" nextWpIdx in
    teleport self $ nextWaypointQuery;
    wait 1000;
    visitNextWaypoint $ nextWpIdx + 1;
    end;

visitNextWaypoint 0;
