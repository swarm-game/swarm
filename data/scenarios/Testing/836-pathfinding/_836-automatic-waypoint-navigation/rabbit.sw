
def visitNextWaypoint = \nextWpIdx.
    emptyHere <- isempty;
    if emptyHere {
      try {
        place "egg";
      } {};
    } {};
    watch down;
    nextWaypointQuery <- waypoint "wp" nextWpIdx;
    teleport self $ snd nextWaypointQuery;
    wait 1000;
    visitNextWaypoint $ nextWpIdx + 1;
    end;

visitNextWaypoint 0;