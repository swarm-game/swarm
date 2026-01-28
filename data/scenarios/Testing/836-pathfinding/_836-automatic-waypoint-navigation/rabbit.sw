
def visitNextWaypoint : (rec l. Unit + (Int * Int) * l) -> (rec l. Unit + (Int * Int) * l) -> Cmd Unit = \originalList. \remainingList.

    emptyHere <- isempty;
    if emptyHere {
      try {
        place "egg";
      } {};
    } {};
    watch down;

    // Wrap around
    let myList = case remainingList (\_. originalList) (\_. remainingList) in

    case myList pure (\cons.
        match cons \wp. \tl.
        teleport self wp;
        wait 1000;

        visitNextWaypoint originalList tl;
    );


    end;

def go =
  let wps = waypoints "wp" in
  visitNextWaypoint wps wps;
end
